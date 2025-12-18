import os
import shlex
import shutil
import subprocess as sp
from glob import glob
from contextlib import contextmanager
from pathlib import Path

GCC_OPTIONS = [
    "-Wall",
    "-Wextra",
    # "-Werror",
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
    "-O0",
    "-lfl",
    "-ggdb",
]

OUTPUT_BINARY = "c-c"
OUTPUT_BINARY_EXT = ".exe" if os.name == "nt" else ""

project_dir = Path(__file__).parent
dist_dir = project_dir / "dist"
src_dir = project_dir / "src"


@contextmanager
def pushd(cd):
    old = os.getcwd()
    os.chdir(cd)
    yield
    os.chdir(old)


def run_command(cmd):
    print(">", shlex.join(map(str, cmd)))
    retcode = sp.call(cmd)
    if retcode != 0:
        exit(f"Falha de build: retornou {retcode}")


def fast_lex():
    with pushd(src_dir):
        command = ["flex", "--yylineno", "lexer.l"]
        run_command(command)


def gcc():
    output_binary_filename = OUTPUT_BINARY + OUTPUT_BINARY_EXT
    output_binary_path = dist_dir / output_binary_filename
    source_files_pattern = str(src_dir).rstrip("/") + "/**.c"
    source_files = list(glob(source_files_pattern, recursive=True))
    command = ["gcc"] + GCC_OPTIONS + source_files + ["-o", str(output_binary_path)]
    run_command(command)


def main():
    shutil.rmtree(dist_dir, ignore_errors=True)
    dist_dir.mkdir()
    fast_lex()
    gcc()


if __name__ == "__main__":
    main()
