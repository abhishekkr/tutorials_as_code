#!/usr/bin/env python

from pprint import pprint
from bs4 import BeautifulSoup
from urllib2 import urlopen

def tracer():
    import pdb; pdb.set_trace()

BASE_URL = "http://static.nvd.nist.gov/feeds/xml/cve/nvdcve-2.0-recent.xml"

def make_soup(url):
    html = urlopen(url).read()
    return BeautifulSoup(html, "lxml")

def get_vuln_reference(node):
    return [patch_refs["href"] for patch_refs in node.findAll("vuln:reference")]

def get_CVE_references(url, cve_id):
    soup = make_soup(url)
    cve = soup.find("entry", id=cve_id)
    return [vuln_refs["href"] for vuln_refs in cve.findAll("vuln:reference")]

def get_by_reference_type(soup, reference_type):
    by_reference_type = soup.findAll("vuln:references", reference_type=reference_type)
    return [get_vuln_reference(item) for item in by_reference_type]

#print get_CVE_references(BASE_URL, "CVE-2011-4106")
print "*"*100
reference_types = ["PATCH", "VENDOR_ADVISORY", "UNKNOWN"]
soup = make_soup(BASE_URL)
for reference_type in reference_types:
    print "For Reference Type: %s" % reference_type
    pprint(get_by_reference_type(soup, reference_type))
    print "-"*100
