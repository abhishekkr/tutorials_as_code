
> APNIC elearning, 2014
> by, Sheryl Hermoso

## IPSec Basics

### VPN

* creates an encrypted tunnel over public network
> * clients to firewall
> * router to router
> * firewall to firewall

* Protocols
> * PPTP (Point To Point Tunneling)
> * L2F (Layer 2 Forwarding)
> * L2TP (Layer 2 Tunneling Protocol)
> * IPSec (Internet Protocol Security)

---

### IPSec

* Layer 3 security (RFC 2401)
> transparent to applications, no IPSec integration required
> uses set of protocols and algorithms to encrypt

* Combines different components
> SA  - Security Association
> AH  - Authentication Headers
> ESP - Encapsulating Security Payload
> IKE - Internet Key Exchange

* Security context of VPN tunnel is established via `ISAKMP` (Internet Security Association and Key Management Protocol)

* Why?
> IP is inherently insecure, all hosts in network are known.
> Issues:
> * Source spoofing
> * Replay packets
> * No data integrity or confidentiality

* What to read?
> * RFC 4301 - defines original IPSec arch and elements
> * RFC 4302 - defines AH
> * RFC 4303 - defines ESP
> * RFC 2408 - defines ISAKMP
> * RFC 5996 - **IKEv2**
> * RFC 4835 - algo for ESP and AH

* Benefits
> * Confidentiality
> * Integrity
> * Authentication
> designed to provide interoperable, high quality, cryptographically-based security for IPv4 and IPv6
> * (optional) anti-replay protection
> * key managment (IKE, revoke/regen of keys, multiple options for auth)

---

### Different Layers of Encryption

```
       AppLayer - SSL, PGP, SSH, HTTPS
  [s]|<------------------------------->|[d]
  ===--,                              ,-===
source |<---------------------------->|destination
      _|_     N/w Layer - IPSec      _|_
     (<->)                          (<->)
     '---'                          '---'
       |                              |
       '-------[=]=[=]=[=]------------'
                |<-->|
            Link Layer Encryption
```

---

### IPSec Modes

* Tunnel Mode
> entire IP packet is encrypted, becomes data component of new+larger IP packet
>
> used in IPSec site-to-site VPN

* Transparent Mode
> IPSec header is inserted into same IPpacket
>
> used in remote-access VPNs

Tunnel Key Management - IKE
Security Protocols - AH and ESP

---

### Security Association (SA)

uniquely identified by
* SPI (Security Parameter Index)
* IP destination
* Security Protocol (AH and/or ESP) identifier

> An SA is unidirectional. 2 SAs for bi-directional communication. So, if using AH and ESP both, it will 4 SAs for one channel.

> Can set-up SA manually or autoamted (using IKE)

#### ISAKMP

* used for establishing SA and crypto keys


#### AK

* provides source auth and data integrity, not confidentiality

* AH follows ESP, if both used

* IP protocol 51


### ESP

* IP protocol 50

* all, auth+integrity+confidentiality

---

### IKE

* phases
> 1 - establish `ISAKMP SA` secure channel, authenticate peers using certs or shared secret
> 2 - establish secure tunnel between peers

* modes
> Main Mode - (phase1) 3 exchanges (2 messages each) between peers, initiator sends one/more proposal and responder selects
> Aggressive Mode - (phase1) Main Mode in 3packets
> Quick Mode - (used for phase2, for tunnel) negotiates params of IPSec session within protection of ISAKMP session

---

### Best Practices

* Use ESP (not AH)
* Use AES (not 3DES)
* Use SHA (not md5)
* reduce lifetime of SA by enabling PFS(Perfect Forward Secrecy), increases CPU load... so only if required

---
---
