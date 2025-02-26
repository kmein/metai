import csv
import itertools
import xml.etree.ElementTree as ET
import sys

file_path = sys.argv[1]


def read_csv(file_path):
    with open(file_path, mode="r", newline="", encoding="utf-8") as csvfile:
        reader = csv.DictReader(csvfile)
        return [row for row in reader]


def write_csv(file_path, fieldnames, rows):
    with open(file_path, mode="w", newline="", encoding="utf-8") as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def transform_metre(metre):
    if isinstance(metre, str):
        return metre.replace("⏑", "-").replace("–", "+").replace("×", "?")
    return metre


def transform_scansion(scansion):
    if isinstance(scansion, str):
        return scansion.replace("D", "+--|").replace("S", "+-|").removesuffix("|")
    return scansion


def insert_dividers(reference, target):
    result = []
    target_index = 0

    if isinstance(target, str) and isinstance(reference, str):
        for char in reference:
            if char == "|":
                result.append("|")
            else:
                if target_index < len(target):
                    result.append(target[target_index])
                    target_index += 1

        return "".join(result)
    else:
        return target


def insert_word_boundaries(syllable_counts, schema):
    syllable_counts = [int(n) for n in syllable_counts.split(".")]
    cumsum = list(itertools.accumulate(syllable_counts))

    result = ""
    syllable_index = 0

    for char in schema:
        result += char
        if char in {"-", "+", "?"}:
            syllable_index += 1
            if syllable_index in cumsum:
                result += "."

    return result.removesuffix(".")


# Read the CSV file
data = read_csv(file_path)

# Transform the data
for row in data:
    row["metre"] = transform_metre(row["metre"])
    row["weight"] = transform_metre(row["weight"])
    row["stress"] = transform_metre(row["stress"])
    row["scansion"] = transform_scansion(row["scansion"])


def combine_metrical_info(row):
    if (
        isinstance(row["metre"], str)
        and isinstance(row["stress"], str)
        and isinstance(row["weight"], str)
    ):
        return insert_word_boundaries(
            row["wordSyllables"],
            insert_dividers(
                row["scansion"],
                "".join(
                    m if m != "?" else (s if s != "?" else w)
                    for m, s, w in zip(row["metre"], row["stress"], row["weight"])
                ),
            ),
        )
    return None  # Return None if any value is missing


# Apply the combination function
for row in data:
    row["combined"] = combine_metrical_info(row)

# Prepare the transformed data for output
output_data = []
for row in data:
    output_row = {"id": row["id"], "met": row["scansion"], "real": row["combined"]}
    output_data.append(output_row)

# Write the transformed data to a new CSV file
xml_input = sys.stdin.read()
tree = ET.ElementTree(ET.fromstring(xml_input))
root = tree.getroot()

# Convert ID column to a dictionary for quick lookup
id_to_attributes = {row["id"]: {"met": row["met"], "real": row["real"]} for row in output_data}

# Iterate through XML elements and add attributes
for elem in root.iter():
    xml_id = elem.get(
        "{http://www.w3.org/XML/1998/namespace}id"
    )  # Get xml:id attribute
    if xml_id and xml_id in id_to_attributes:
        if isinstance(id_to_attributes[xml_id]["met"], str):
            elem.set("met", id_to_attributes[xml_id]["met"])
        if isinstance(id_to_attributes[xml_id]["real"], str):
            elem.set("real", id_to_attributes[xml_id]["real"])

tei_header = root.find(".//teiHeader")
if tei_header is not None:
    encoding_desc = tei_header.find(".//encodingDesc")
    if encoding_desc is None:
        encoding_desc = ET.SubElement(tei_header, "encodingDesc")

    met_decl = ET.SubElement(
        encoding_desc, "metDecl", type="met", pattern=r"(((\+--|\+-)\|){5}\+-)"
    )
    ET.SubElement(met_decl, "metSym", value="+").text = "heavy syllable"
    ET.SubElement(met_decl, "metSym", value="-").text = "light syllable"
    ET.SubElement(met_decl, "metSym", value="|").text = "foot boundary"
    met_decl = ET.SubElement(
        encoding_desc, "metDecl", type="real", pattern="([+-|?.]+)"
    )
    ET.SubElement(met_decl, "metSym", value="+").text = "heavy syllable"
    ET.SubElement(met_decl, "metSym", value="-").text = "light syllable"
    ET.SubElement(met_decl, "metSym", value="?").text = "syllable of unknown quantity"
    ET.SubElement(met_decl, "metSym", value=".").text = "word boundary"
    ET.SubElement(met_decl, "metSym", value="|").text = "foot boundary"

# Save modified XML
output_xml = ET.tostring(root, encoding='utf-8', xml_declaration=True)
sys.stdout.buffer.write(output_xml)
print()
