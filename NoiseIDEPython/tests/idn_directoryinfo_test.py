import time

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import unittest
import os
import time

class TestDirectoryInfoDiff(unittest.TestCase):

    def test_time(self):
        start = time.time()
        result = []
        result.append("D:/Projects/Test")
        for root, dirnames, filenames in os.walk("D:/Projects/Test"):
            for filename in filenames:
                result.append(os.path.join(root, filename))
            for dirname in dirnames:
                result.append(os.path.join(root, dirname))
        end = time.time()
        print (end - start) * 1000, len(result)

if __name__ == '__main__':
    unittest.main()