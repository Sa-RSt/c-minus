import os
import subprocess as sp
from glob import glob
from pathlib import Path
import shlex

GCC_OPTIONS = [
    "-Wall",
    "-Wextra",
    "-Werror",
    "-Wdouble-promotion",
    "-Wpointer-to-int-cast",
    "-Wint-to-pointer-cast",
    "-Wshadow",
    "-Wchar-subscripts",
    "-Wpointer-arith",
    "-Wcast-qual",
    "-Wstrict-prototypes",
    "-Wmissing-prototypes",
    "-Wuninitialized",
    "-Wmisleading-indentation",
    "-Wswitch-enum",
    "-Wswitch-default",
    "-Wconversion",
    "-O2",
]

OUTPUT_BINARY = "c-c"
OUTPUT_BINARY_EXT = ".exe" if os.name == "nt" else ""

project_dir = Path(__file__).parent


def main():
    source_files_pattern = str(project_dir).rstrip("/") + "/**.c"
    source_files = list(glob(source_files_pattern, recursive=True))
    command = (
        ["gcc"] + GCC_OPTIONS + source_files + ["-o", OUTPUT_BINARY + OUTPUT_BINARY_EXT]
    )
    print(">", shlex.join(command))
    retcode = sp.call(command)
    if retcode != 0:
        print(f"Falha de compilação: GCC retornou {retcode}")


if __name__ == "__main__":
    main()
