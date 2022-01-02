use std::ops::Mul;

use crate::{
    llir_nodes::{BinaryOperation, Node},
    opt::{
        global_opt::Commands,
        optimize_commands::{OptimizeCommand, OptimizeCommandKind},
    },
    utils::{ScoreboardOperation, ScoreboardValue},
};

/// Optimizes basic arithmetic expressions with the shape
/// x OP constant OP constant ...
/// NOTE: This optimizer changes the runtime behavior!
/// An operation like `a * 2000 / 1000` can cause an overflow,
/// while the optimized version `a * 2` might not.
/// For this reason it is **very important** that this optimizer is
/// not toggled between release and debug mode.
pub fn simple_arithmetic_optimization(commands: &mut Commands) {
    'functions: for function in commands.optimizer.iter_functions() {
        for (node_id, node) in function {
            if let Node::BinaryOperation(BinaryOperation {
                scoreboard,
                id: target,
                lhs: ScoreboardValue::Scoreboard(lhs_scoreboard, lhs_id),
                rhs: ScoreboardValue::Static(value),
                operation,
            }) = node
            {
                if matches!(
                    operation,
                    ScoreboardOperation::Plus | ScoreboardOperation::Minus
                ) {
                    let mut subsequent_nodes = commands
                        .optimizer
                        .iter_at(&node_id)
                        .map(|(_, other_node)| {
                            if let Node::BinaryOperation(BinaryOperation {
                                scoreboard: _,
                                id: new_target,
                                lhs: ScoreboardValue::Scoreboard(_, also_new_target),
                                rhs: ScoreboardValue::Static(new_value),
                                operation,
                            }) = other_node
                            {
                                if new_target == also_new_target
                                    && new_target == target
                                    && matches!(
                                        operation,
                                        ScoreboardOperation::Plus | ScoreboardOperation::Minus
                                    )
                                {
                                    return Some(match operation {
                                        ScoreboardOperation::Plus => *new_value,
                                        ScoreboardOperation::Minus => -new_value,
                                        _ => unreachable!(),
                                    });
                                }
                            }
                            None
                        })
                        .take_while(Option::is_some)
                        .map(Option::unwrap)
                        .peekable();
                    if subsequent_nodes.peek().is_some() {
                        let (optimized_value, len) =
                            optimize_additive(std::iter::once(*value).chain(subsequent_nodes));
                        for i in 0..((len - 1) as usize) {
                            commands.commands.push(OptimizeCommand::new(
                                (node_id.0, node_id.1 + i),
                                OptimizeCommandKind::Delete,
                            ));
                        }
                        commands.commands.push(OptimizeCommand::new(
                            node_id,
                            OptimizeCommandKind::Replace(Node::BinaryOperation(BinaryOperation {
                                scoreboard: *scoreboard,
                                id: *target,
                                lhs: ScoreboardValue::Scoreboard(*lhs_scoreboard, *lhs_id),
                                rhs: ScoreboardValue::Static(optimized_value),
                                operation: *operation,
                            })),
                        ));
                        continue 'functions;
                    }
                } else if matches!(
                    operation,
                    ScoreboardOperation::Times | ScoreboardOperation::Divide
                ) {
                    let original_fraction = match operation {
                        ScoreboardOperation::Times => Fraction::from(*value),
                        ScoreboardOperation::Divide => Fraction::from(*value).inverse(),
                        _ => unreachable!(),
                    };
                    let mut subsequent_nodes = commands
                        .optimizer
                        .iter_at(&node_id)
                        .map(|(_, other_node)| {
                            if let Node::BinaryOperation(BinaryOperation {
                                scoreboard: _,
                                id: new_target,
                                lhs: ScoreboardValue::Scoreboard(_, also_new_target),
                                rhs: ScoreboardValue::Static(new_value),
                                operation,
                            }) = other_node
                            {
                                if new_target == also_new_target
                                    && new_target == target
                                    && matches!(
                                        operation,
                                        ScoreboardOperation::Times | ScoreboardOperation::Divide
                                    )
                                {
                                    return Some(match operation {
                                        ScoreboardOperation::Times => Fraction::from(*new_value),
                                        ScoreboardOperation::Divide => {
                                            Fraction::from(*new_value).inverse()
                                        }
                                        _ => unreachable!(),
                                    });
                                }
                            }
                            None
                        })
                        .take_while(Option::is_some)
                        .map(Option::unwrap)
                        .peekable();
                    if subsequent_nodes.peek().is_some() {
                        let (optimized_value, len) = optimize_multiplicative(
                            std::iter::once(original_fraction).chain(subsequent_nodes),
                        );
                        let (new_operation, new_value) = match optimized_value {
                            Fraction {
                                numerator,
                                denominator: 1,
                            } => (ScoreboardOperation::Times, numerator),
                            Fraction {
                                numerator: 1,
                                denominator,
                            } => (ScoreboardOperation::Divide, denominator),
                            _ => break,
                        };
                        for i in 0..((len - 1) as usize) {
                            commands.commands.push(OptimizeCommand::new(
                                (node_id.0, node_id.1 + i),
                                OptimizeCommandKind::Delete,
                            ));
                        }
                        commands.commands.push(OptimizeCommand::new(
                            node_id,
                            OptimizeCommandKind::Replace(Node::BinaryOperation(BinaryOperation {
                                scoreboard: *scoreboard,
                                id: *target,
                                lhs: ScoreboardValue::Scoreboard(*lhs_scoreboard, *lhs_id),
                                rhs: ScoreboardValue::Static(new_value),
                                operation: new_operation,
                            })),
                        ));
                        continue 'functions;
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
struct Fraction {
    numerator: i32,
    denominator: i32,
}

impl Fraction {
    const ONE: Fraction = Fraction {
        numerator: 1,
        denominator: 1,
    };

    fn simplify(&mut self) {
        let mut a = self.numerator;
        let mut b = self.denominator;
        while b != 0 {
            let temp = b;
            b = a % b;
            a = temp;
        }

        self.numerator /= a;
        self.denominator /= a;
    }

    fn inverse(self) -> Fraction {
        Fraction {
            numerator: self.denominator,
            denominator: self.numerator,
        }
    }
}

impl From<i32> for Fraction {
    fn from(val: i32) -> Self {
        Fraction {
            numerator: val,
            denominator: 1,
        }
    }
}

impl Mul for Fraction {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut result = Fraction {
            numerator: self.numerator * rhs.numerator,
            denominator: self.denominator * rhs.denominator,
        };
        result.simplify();
        result
    }
}

fn optimize_additive(operations: impl Iterator<Item = i32>) -> (i32, u32) {
    operations.fold((0, 0), |(acc, len), x| {
        (ScoreboardOperation::Plus.evaluate(acc, x), len + 1)
    })
}

fn optimize_multiplicative(operations: impl Iterator<Item = Fraction>) -> (Fraction, u32) {
    operations.fold((Fraction::ONE, 0), |(acc, len), x| (acc * x, len + 1))
}
