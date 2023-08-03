import urllib.request
# native python elementtree has no support for xpath |
from lxml import etree as ET
from typing import List
import re
import csv
import sys

prefix_map = {"xml": "http://www.w3.org/XML/1998/namespace"}
parser = ET.XMLParser(recover=True)

books = ["PL", "WD", "RG", "ZR"]

corpus_urls = [
    "https://titus.fkidg1.uni-frankfurt.de/cordon/data/tei/DM_PL_1765_1775.tei.xml",
    "https://titus.fkidg1.uni-frankfurt.de/cordon/data/tei/DM_WD_1765_1775.tei.xml",
    "https://titus.fkidg1.uni-frankfurt.de/cordon/data/tei/DMN_RG_1869.tei.xml",
    "https://titus.fkidg1.uni-frankfurt.de/cordon/data/tei/DMN_ZR_1869.tei.xml",
]

def get_corpus() -> List[ET.Element]:
    result = []
    for corpus_url in corpus_urls:
        request = urllib.request.urlopen(corpus_url)
        xml_text = request.read()
        request.close()
        result.append(ET.fromstring(xml_text, parser=parser))
    return result

def lines(corpus: ET.Element) -> List[ET.Element]:
    return corpus.findall(".//body/l", namespaces=prefix_map)

cleft_word = re.compile(r'(\S*)_1 \1_2')

def evaluate(line: ET.Element) -> tuple[str, str]:
    xml_id = line.attrib[f"{{{prefix_map['xml']}}}id"]
    words = line.xpath("./unit/w[@xml:lang='olt' or @xml:lang='oli' or @xml:lang='olit' or @xml:lang='olt olt' or @xml:lang='']|./pc", namespaces=prefix_map)
    text = re.sub(
        pattern='„ ',
        repl='„',
        string=re.sub(
          pattern=' ([:;“,!?.])',
          repl=r'\1',
          string=cleft_word.sub(
            string=" ".join(unit.text or "???" for unit in words),
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
    donelaitis_writer = csv.DictWriter(sys.stdout, fieldnames=["Book", "Line", "Text"])
    donelaitis_writer.writeheader()
    for book in get_corpus():
        for line in lines(book):
            xml_id, text = evaluate(line)
            try:
                if text in ["Ganà.", "ganà."]: continue
                book_id, page, page_line, line = re.sub(pattern='####nuo čia: .*', repl='', string=xml_id).split("_")
                donelaitis_writer.writerow({
                    "Book": books.index(book_id)+1,
                    "Line": line,
                    "Text": annotate_syllable_boundaries(text),
                })
            except:
                print(xml_id, file=sys.stderr)
