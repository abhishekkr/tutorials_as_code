import ctypes
import os

def main():
  # Path to shared object file
  path = os.path.join(os.path.abspath("."), "go_go_go.so")

  # load shared object file
  lib = ctypes.CDLL(path)

  # call shared object method
  #import pdb ; pdb.set_trace();
  lib.go_go_go()


if __name__ == "__main__":
  main()
