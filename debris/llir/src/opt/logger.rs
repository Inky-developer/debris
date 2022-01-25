//! Simple logger to make debugging the optimizer simpler. Can be enabled using a flag.

#[cfg(feature = "log_optimizer")]
pub const LOG_ENABLED: bool = true;
#[cfg(not(feature = "log_optimizer"))]
pub const LOG_ENABLED: bool = false;

#[macro_export]
macro_rules! log {
    ($($args:tt)*) => {
        if $crate::opt::logger::LOG_ENABLED {
            print!("{}:{}:{} ", file!(), line!(), column!());
            println!($($args)*);
        }
    };
}
