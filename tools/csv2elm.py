#!/usr/bin/env python
import sys
import os
import csv
import argparse


def create_scons_target(env, input_folder, output_folder, generated_files):
    def operation(input_folder, output_folder):
        return lambda target, source, env: main(input_folder, output_folder)

    return env.Command(generated_files, env.Glob(f"{input_folder}/*.csv"), operation(input_folder, output_folder))


def main(indir, outdir):
    def cap(s):
        return s[0].upper() + s[1:]

    print(f"Generazioni da {indir} a {outdir} ...")
    files = [x for x in [os.path.join(indir, y) for y in os.listdir(
        indir)] if os.path.isfile(x) and x.endswith('.csv')]
    translations = {}
    languages = []

    for csvfile in files:
        with open(csvfile, 'r') as f:
            csvreader = csv.reader(f, delimiter=',', skipinitialspace=True)
            arrayname = os.path.basename(csvfile).replace(".csv", "")
            tmp = {}

            first = next(csvreader)  # Drop the first line
            languages = first[1:]

            for line in csvreader:
                if len(line) < 2:
                    print("Devono esserci almeno due colonne (la prima e' per l'enum)")
                    exit(1)
                tmp[line[0]] = [x.lstrip() for x in line[1:]]

            translations[arrayname] = tmp

    try:
        name = os.path.basename(indir)
        module = f"AUTOGEN_FILE_{name}"

        with open(os.path.join(outdir, f"{module}.elm"), 'w') as elm:
            elm.write(f"module {module} exposing (..)\n\n\n")

            language_type = "type Language\n"
            translation_type = "type alias Translation =\n    { "
            language_to_string_fun = "languageString : Language -> String\nlanguageString language =\n    case language of\n"
            language_from_string_fun = "languageFromString : String -> Language\nlanguageFromString string =\n    case string of\n"
            get_translation_fun_header = "getTranslation : Language -> Translation -> String\ngetTranslation language {"
            get_translation_fun_body = "    case language of\n"
            set_translation_fun = "setTranslation : Translation -> Language -> String -> Translation\nsetTranslation translation language string =\n    case language of\n"

            for i in range(len(languages)):
                if i == 0:
                    language_type += f"    = {cap(languages[0])}\n"
                else:
                    language_type += f"    | {cap(languages[i])}\n"

                if i == len(languages) - 1:
                    get_translation_fun_header += f" {languages[i].lower()} }} =\n"
                    translation_type += f"{languages[i].lower()} : String }}\n"
                else:
                    get_translation_fun_header += f" {languages[i].lower()},"
                    translation_type += f"{languages[i].lower()} : String, "

                language_to_string_fun += f"        {cap(languages[i])} ->\n            \"{languages[i]}\"\n\n"
                language_from_string_fun += f"        \"{languages[i]}\" ->\n            {languages[i]}\n\n"
                get_translation_fun_body += f"        {cap(languages[i])} ->\n            {languages[i].lower()}\n\n"
                set_translation_fun += f"        {cap(languages[i])} ->\n            {{ translation | {languages[i].lower()} = string }}\n\n"

            language_from_string_fun += f"        _ ->\n            {languages[0]}\n\n"

            elm.write(language_type)
            elm.write("\n\n")
            elm.write(translation_type)
            elm.write("\n\n")
            elm.write(language_to_string_fun)
            elm.write("\n")
            elm.write(language_from_string_fun)
            elm.write("\n")
            elm.write(get_translation_fun_header)
            elm.write(get_translation_fun_body)
            elm.write("\n")
            elm.write(set_translation_fun)
            elm.write("\n")

            intl_string_type = "type IntlString\n"
            translate_fun = "translate : Language -> IntlString -> String\ntranslate language intlString =\n    case intlString of\n"
            first = True

            for filename, value in translations.items():
                items = len(value.keys())
                lingue = len(list(value.values())[0])

                for enum in value.keys():
                    if len(value[enum]) != lingue:
                        print(
                            f"Numero di lingue diverso nel file {filename}.csv: {lingue} vs {len(value[enum])}")
                        exit(1)

                    if first:
                        intl_string_type += f"    = {cap(enum)}\n"
                        first = False
                    else:
                        intl_string_type += f"    | {cap(enum)}\n"

                    translate_fun += f"        {cap(enum)} ->\n            getTranslation language <| Translation"
                    for translation in value[enum]:
                        translate_fun += f" \"{translation}\""
                    translate_fun += "\n\n"

                elm.write(intl_string_type)
                elm.write("\n\n")
                elm.write(translate_fun)

    except EnvironmentError as e:
        print(e)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description="Generazione automatica di ADT Elm di traduzioni")
    parser.add_argument('cartella', type=str,
                        help='Cartella dove trovare i file .csv')
    parser.add_argument('-o', '--output', type=str, nargs='?', default='.',
                        help='Cartella dove vengono salvati i sorgenti generati')
    args = parser.parse_args()

    main(args.cartella, args.output)
