import re

pattern = r"((([01]?[0-9]?[0-9]|2[0-4][0-9]|25[0-5])[ (\[]?(\.|dot)[ )\]]?){3}([01]?[0-9]?[0-9]|2[0-4][0-9]|25[0-5]))"

text = "The following are IP addresses: 192.168.1.1, 8.8.8.8, 101.099.098.000. These can also appear as 192.168.1[.]1 or 192.168.1(.)1 or 192.168.1[dot]1 or 192.168.1(dot)1 or 192 .168 .1 .1 or 192. 168. 1. 1. "
ips = [match[0] for match in re.findall(pattern, text)]
print ips


public_ips = ["fe80::120b:a9ff:fecd:514", "10.104.100.72"]
private_ips = ["10.104.100.73", "fe50::120b:a9ff:fecd:514"]
ips = [ip for ip in public_ips if re.match(pattern, ip)]
print ips
ips = [ip for ip in private_ips if re.match(pattern, ip)]
print ips
