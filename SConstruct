import os
from shutil import copyfile
import tools.csv2elm as csv2elm


BUILD = os.path.abspath("build")
DIST = "dist"
ELMSRC = "src-elm"
ELMJS = os.path.join(BUILD, "elm.js")
MINELMJS = os.path.join(BUILD, "elm.min.js")

folders = [
    os.path.join(".", "node_modules", ".bin"),
    os.path.join(".", "node_modules", "elm", "bin")
]
binfolder = None
for folder in folders:
    if os.path.isdir(folder):
        binfolder = os.path.abspath(folder)
        break
if not binfolder:
    binfolder = folders[0]

UGLIFYJS = os.path.join(binfolder, "uglifyjs")
ELM = os.path.join(binfolder, "elm")

env_options = {
    "ENV": os.environ,
}
env = Environment(**env_options)

AddOption('--release',
          dest='release',
          action='store_true',
          help='release build')

elmsrc = Glob(f"{ELMSRC}/src/**/**/**/*.elm")
elmsrc += Glob(f"{ELMSRC}/src/**/**/*.elm")
elmsrc += Glob(f"{ELMSRC}/src/**/*.elm")
elmsrc += Glob("{ELMSRC}/src/*.elm")

env.Command(ELM, [], 'npm install elm')
env.Command(UGLIFYJS, [], 'npm install uglify-es')

intl = csv2elm.create_scons_target(env, "assets/translations",
                                   "src-elm/src", "src-elm/src/AUTOGEN_FILE_translations.elm")


elmfiles = [os.path.join("src", "Pages", "Machine", "Page.elm")]

env.Command(ELMJS, [elmsrc, ELM, intl],
            f"cd {ELMSRC} && {ELM} make {' '.join(elmfiles)} {'--optimize' if GetOption('release') else ''} --output={ELMJS}")


if GetOption("release"):
    minelmjs = env.Command(
        MINELMJS, [UGLIFYJS, ELMJS],
        f"{UGLIFYJS} {ELMJS} --compress 'pure_funcs=\"F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9\",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | {UGLIFYJS} --mangle --output={MINELMJS}"
    )
else:
    def copyelm(*args, **kwargs):
        copyfile(ELMJS, MINELMJS)

    minelmjs = env.Command(
        MINELMJS, ELMJS,
        copyelm
    )


def copyelm(*args, **kwargs):
    copyfile(MINELMJS, f"{DIST}/{os.path.basename(MINELMJS)}")


final = env.Command(f"{DIST}/{os.path.basename(MINELMJS)}", MINELMJS, copyelm)

env.Default(final)
env.NoClean([ELM, UGLIFYJS])
