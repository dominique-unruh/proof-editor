from PyQt5.QtWidgets import QApplication
from PyQt5.QtSvg import QSvgRenderer
from PyQt5.QtCore import QCoreApplication, QEventLoop, QEvent
import sys
import time
import logging_conf  # @UnusedImport

sys.path += ['..']

from unittest import TestCase
from mathview import Formula
from unittest.mock import Mock # test:ignore
from lxml import etree  # @UnresolvedImport

qapplication_singleton = None
class TestQApplication(TestCase):
    def __init__(self, x):
        super().__init__(x)
        print("INIT")
        self._exitLoop = False

    def setUp(self):
        super().setUp()
        global qapplication_singleton
        if qapplication_singleton is None:
            print("Creating QApplication")
            qapplication_singleton = QApplication([])
        self._exitLoop = False

    def getQApplication(self):
        global qapplication_singleton
        assert qapplication_singleton is not None
        return qapplication_singleton

    def eventLoop(self,timeout=1):
        start = time.time()
        end = start+timeout
        while not self._exitLoop:
            self.assertTrue(time.time() <= end, "eventLoop reached timeout")
            print("Processing events")
            QCoreApplication.processEvents(QEventLoop.AllEvents, timeout);
            QCoreApplication.sendPostedEvents(None, QEvent.DeferredDelete);
            print("Processing events done")
            time.sleep(0.01)
        self._exitLoop = False
        
    def exitLoop(self):
        self._exitLoop = True

class Test(TestQApplication):
#     def setUp(self):
#         print("setUp",PyQt5.QtWidgets.qApp)
#         if PyQt5.QtWidgets.qApp == None: 
#             print("Creating QApplication")
#             QApplication([])
#         pass
    
    def test_pmml(self):
        math = Formula("x y ^",format='auto')
        pmml = math.get_pmathml()
        # Minimal wellformedness test
        dom = etree.fromstring(pmml)
        self.assertEqual(dom.tag, 'msup')
    
        
    
    def test_svg(self):
        cb = Mock(side_effect=lambda *x: self.exitLoop())
        math = Formula("example5",format='auto')
        math.get_svg(cb)
        
        self.eventLoop()

        assert cb.called
        svg = cb.call_args[0][0]
        renderer = QSvgRenderer()
        success = renderer.load(etree.tostring(svg))
        print("rendering done")
        print(etree.tostring(svg))
        self.assertTrue(success)
        self.assertEqual(svg.tag, '{http://www.w3.org/2000/svg}svg')
        
    def test_svg_many(self):
        num = 10
        def decrease(*x):
            nonlocal num
            print("decrease",num)
            num -= 1
            if num==0: self.exitLoop()
        cb = [None for i in range(num)]
        for i in range(num):
            print(i)
            cb[i] = Mock(side_effect=decrease)
            math = Formula("x y ^",format='auto')
            svg = math.get_svg(cb[i])
        
        
        self.eventLoop(timeout=10)
        
        print("exec done")
        for i in range(num):
            print("X",cb[i].called)
            assert cb[i].called, i
            svg = cb[i].call_args[0][0]
            dom = etree.fromstring(svg)
            self.assertEqual(dom.tag, '{http://www.w3.org/2000/svg}svg')
    
