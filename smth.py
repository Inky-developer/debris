from os import walk
from os.path import join

total_lines = 0
for dirpath, dirnames, filenames in walk("debris"):
	for file in (join(dirpath, i) for i in filenames if i.endswith(".rs")):
		file_lines = 0
		with open(file) as f:
			for line in f.readlines():
				if line.strip():
					file_lines += 1

		print(f"{file}: {file_lines} lines")
		total_lines += file_lines


print(f"total lines: {total_lines}")