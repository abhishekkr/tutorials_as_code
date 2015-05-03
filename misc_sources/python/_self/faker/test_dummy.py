#!/usr/bin/env python

import unittest

import os
import sys

sys.path.append(os.path.dirname(os.path.realpath(__file__)))
import dummy
import faker


class TestDummy(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_u(self):
        faker.fake_it("os.path.isfile", faker.return_pass)
        _path = "/tmp/nopath"
        _u = dummy.u(_path)
        self.assertEqual(_u, 3)
        faker.real_it("os.path.isfile")
        _fake_count = faker.get_fake_call("return_pass,u,test_u", _path)
        self.assertEqual(_fake_count, 1)

        faker.fake_it("os.path.isfile", faker.return_true)
        _path = "/tmp/somepath"
        _u = dummy.u(_path)
        self.assertEqual(_u, 1)
        faker.real_it("os.path.isfile")
        _fake_count = faker.get_fake_call("return_true,u,test_u", _path)
        self.assertEqual(_fake_count, 1)


if __name__ == '__main__':
    unittest.main()

