import time

__author__ = 'Yaroslav Nikityshev aka IDNoise'

import unittest
import os
import shutil
from idn_directoryinfo import DirectoryInfo, DirectoryInfoDiff

class TestDirectoryInfoDiff(unittest.TestCase):
    def setUp(self):
        self.root = "D:\\Temp"
        self.createFile('test_file1.test')
        self.createFile('test_file2.test')
        self.createDir('dir_test1')
        self.createDir('dir_test2')
        time.sleep(1)
        self.data = DirectoryInfo(self.root)

    def tearDown(self):
        time.sleep(1)
        self.deleteFile('test_file1.test')
        self.deleteFile('test_file3.test')
        self.deleteDir('dir_test3')
        self.deleteDir('dir_test2')

    def test_diff(self):
        self.createFile('test_file3.test')
        self.modifyFile('test_file1.test')
        self.deleteFile('test_file2.test')

        self.createDir('dir_test3')
        self.deleteDir('dir_test1')
        self.modifyDir('dir_test2')

        newData = DirectoryInfo(self.root)
        diff = DirectoryInfoDiff(newData, self.data)

        self.assertEqual(len(diff.createdFiles), 2)
        self.assertEqual(len(diff.modifiedFiles), 1)
        self.assertEqual(len(diff.deletedFiles), 1)

        self.assertEqual(len(diff.createdDirs), 1)
        self.assertEqual(len(diff.modifiedDirs), 1)
        self.assertEqual(len(diff.deletedDirs), 1)

        import timeit
        t = timeit.Timer("""
from idn_directoryinfo import DirectoryInfo
DirectoryInfo('D:\\Projects\\GIJoe\\server', True)""")
        #print t.timeit(100)

    def createFile(self, name):
        f = file(os.path.join(self.root, name), 'w')
        f.write("lkasjdflkajsdflkajsdlk;fjasdlkf")
        f.close()

    def modifyFile(self, name):
        f = file(os.path.join(self.root, name), 'w')
        f.write("lkasjdflkajsdflkajsdlk;fjaadfasdfasdfasdfasdfasdfsdlkf")
        f.close()

    def deleteFile(self, name):
        name = os.path.join(self.root, name)
        os.remove(name)

    def createDir(self, name):
        os.makedirs(os.path.join(self.root, name))

    def modifyDir(self, name):
        name = os.path.join(self.root, name, 'xx.xxx')
        f = file(name, 'w')
        f.write('xxxx')
        f.close()

    def deleteDir(self, name):
        shutil.rmtree(os.path.join(self.root, name))

if __name__ == '__main__':
    unittest.main()