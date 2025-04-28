
## Quantum Cryptography

> Unlike traditional cryptography that's built on mammoth mathematical complexity to reverse engineer the encryption; Quantum Cryptography depends on Physics Laws.

**It relies on unique principles of quantum mechanics:**

> * Particles can exist in more than one place or in more than one state; to predict there is no means.
> * Photons' spins can be measured randomly in binary positions.
> * Quantum state can't be measured without being altered; as basic act of measuring impacts the state of system.
> * Particles can be partially, but not completely cloned.

### Types of Quantum Cryptography

#### Quantum Key Distribution (QKD)

* QKD ain't used to encrypt data; but secure key exchange between 2 parties by collaboratively building a shared private key similar to one used by symmetric key encryption.

* QKD sends individual photons across an optic cable; each representing a bit (or Qubit). The photon spin series becomes the key. Polarized filters on sender's side change the physical orientation of each photon to a specific spin.

#### Quantum coin-flipping

* A cryptographic primitive allowing two untrusted parties to agree on set of trust parameters.

* Leverages principles of superposition and entanglement, to ensure a fair and secure coin flip.

### Post-Quantum Cryptography

Goal is to have Quantum-resistant cryptography; 6 primary areas:

> * Lattice-based Cryptography
> * Multivariate Cryptography
> * Hash-based Cryptography
> * Code-based Cryptography
> * Isogeny-based Cryptography
> * Symmetric key quantum resistance

**General Encryption**

* [Crytals Kyber](https://pq-crystals.org/kyber/index.shtml) is an IND-CCA2-secure KEM (key encapsulation mechanism), one of the finalists for NIST's Post-Quantum Cryptography Project. Whose security is based on the hardness of solving the LWE (learning with errors) problem over module lattices.

**Digital Signature Scheme**

* [Crystals Dilithium](https://pq-crystals.org/dilithium/index.shtml)

* [Falcon: Fast-Fourier Lattice-based Compact Signatures over NTRU](https://falcon-sign.info/)

* [SPHINCS: stateless hash-based signature scheme](https://sphincs.org/)

---

sources:

[2022: NIST's 4 Quantum Resistant Cryptography Algorithm](https://www.nist.gov/news-events/news/2022/07/nist-announces-first-four-quantum-resistant-cryptographic-algorithms)

---
