# native python elementtree has no support for xpath's binary `|` operator
from lxml import etree
from typing import List
import re
import csv
import sys

prefix_map = {"xml": "http://www.w3.org/XML/1998/namespace"}
parser = etree.XMLParser(recover=True)

books = ["PL", "WD", "RG", "ZR"]

cleft_word = re.compile(r'(\S*)_1 \1_2')


def lines(corpus: etree.Element) -> List[etree.Element]:
    return corpus.findall(".//body/l", namespaces=prefix_map)


def evaluate(line: etree.Element) -> tuple[str, str]:
    xml_id = line.attrib[f"{{{prefix_map['xml']}}}id"]
    words = line.xpath("./unit/w[@xml:lang='olt' or @xml:lang='oli' or @xml:lang='olit' or @xml:lang='olt olt' or @xml:lang='']|./pc", namespaces=prefix_map)
    text = re.sub(
        pattern='„ ',
        repl='„',
        string=re.sub(
            pattern=' ([:;“,!?.])',
            repl=r'\1',
            string=cleft_word.sub(
                string=" ".join(unit.text for unit in words),
                repl=r'\1')
        )
    )
    return xml_id, text


def annotate_syllable_boundaries(text: str) -> str:
    return re.sub('(prĭ|pă|sŭ|ſŭ)(sĭ|ſĭ)(ė|a|è|á|ì|ů)', r'\1\2|\3',
                  re.sub('păì', 'pă|ì',
                         re.sub('prìĕ', 'prì|ĕ',
                                re.sub('priė', 'pri|ė',
                                       re.sub('(s|ſ|n)(ŭ|u)(ė|áu|ů)', r'\1\2|\3',
                                              re.sub('(K|k)ábĭăr', r'\1ábĭ|ăr',
                                                     text))))))


if __name__ == "__main__":
    donelaitis_writer = csv.DictWriter(sys.stdout, fieldnames=["ID", "Book", "Line", "Text"])
    donelaitis_writer.writeheader()
    for path in sys.argv[1:]:
        with open(path, "r") as book_file:
            book = etree.parse(book_file, parser=parser)
            for line in lines(book):
                xml_id, text = evaluate(line)
                try:
                    if text in ["Ganà.", "ganà."]:
                        continue
                    xml_id = re.sub(pattern='####nuo čia: .*', repl='', string=xml_id)
                    book_id, page, page_line, line = xml_id.split("_")
                    donelaitis_writer.writerow({
                        "ID": xml_id,
                        "Book": books.index(book_id) + 1,
                        "Line": line,
                        "Text": annotate_syllable_boundaries(text),
                    })
                except ValueError:  # not enough values to unpack
                    print(xml_id, file=sys.stderr)
