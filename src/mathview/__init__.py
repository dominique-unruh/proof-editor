import logging, os, sys, traceback
from subprocess import Popen, PIPE
from PyQt5.QtWebKitWidgets import QWebPage
from PyQt5.QtCore import pyqtSlot, QUrl, QObject


# TODO: the SVG-page accumulates glyphs. We should occasionally reinitialize SVGConverter to increase efficiency?


# Own modules
import utils

cmathml = utils.file_path("cmathml" if sys.platform!="win32" else "cmathml.exe")
mathedhaskell = utils.file_path("MathEdHaskell" if sys.platform!="win32" else "MathEdHaskell.exe")
base_url = QUrl.fromLocalFile(utils.file_path("resources/math.html"))

class ConverterError(RuntimeError):
    def __init__(self, cmd, errors):
        super().__init__(cmd, errors)
        self.errors = errors
        self.cmd = cmd
def call_converter(command, *args):
    if command in ('tex2cmml','transform'):
        converter = mathedhaskell
    else:
        converter = cmathml

    cmd = [converter, command]+list(args)
    cmdstr = " ".join("'{}'".format(x) for x in cmd)
    logging.info("Calling {}".format(cmdstr))

    try:
        proc = Popen(cmd, cwd=utils.base_path, env={'OCAMLRUNPARAM':'b'},  stdin=PIPE, stdout=PIPE, stderr=PIPE)
        (pmml, errors) = proc.communicate()
    except:
        e = traceback.format_exc()
        logging.warning(e)
        raise ConverterError("Unhandled exception",e)
    if proc.returncode != 0:
        errors = str(errors, encoding='utf-8')
        logging.warning("Converter call failed: {}".format(cmdstr, errors))
        raise ConverterError(cmd, errors)
        
    pmml = str(pmml, encoding="utf-8")
    return pmml

class QWebPageLogging(QWebPage):
    def javaScriptConsoleMessage(self, message, i, sourceID):
        logging.info("{}:{}: {}".format(sourceID, str(i), message))

singleton_SVGConverter = None # type: SVGConverter
class SVGConverter(QObject):
    def __init__(self):
        super().__init__()
        #self.lock = threading.Lock()
        self.webpage = QWebPageLogging()
        self.html_file = utils.file_path("resources/svg_convert.html")
        
#        import __main__
#        win = __main__.win
#        view = QWebView(win)
#        view.setPage(self.webpage);
#        win.setWindowTitle("bla")
#        win.resize(600, 600)
#        win.setCentralWidget(view)
#        print("WIN", win)
        
        self.queue = []
        self.busy = True
        self.error = False
       
        self.webpage.mainFrame().javaScriptWindowObjectCleared.connect(self.javaScriptWindowObjectCleared)
        self.webpage.mainFrame().loadFinished.connect(self.load_finished) # This does not imply that body has been loaded
        self.webpage.mainFrame().setUrl(QUrl.fromLocalFile(self.html_file))
        logging.info(base_url)
        
    @pyqtSlot()
    def javaScriptWindowObjectCleared(self):
        self.webpage.mainFrame().addToJavaScriptWindowObject("controller", self)

    @pyqtSlot(bool)
    def load_finished(self, ok):
        self.error = not ok
        if not ok:
            logging.warning("Loading HTML failed: %s",self.html_file)
        self.webpage.mainFrame().addToJavaScriptWindowObject("controller", self)
        if not ok: # In this case, all callbacks will be called with an error message
            self.busy = False
            self.next_in_queue()
        
    @pyqtSlot()
    def body_load(self):
        self.busy = False
        self.next_in_queue()
        
    def next_in_queue(self):
        if self.busy: return
        if not self.queue: return
        (self._mml_for_js, self._callback) = self.queue.pop(0)
        if self.error:
            self._callback(None)
        self.busy = True
        self.webpage.mainFrame().evaluateJavaScript("convert_to_svgcb()");

    @pyqtSlot(str, str)
    def svg_rendered(self, svgxml, defsxml):
        from lxml import etree # type: ignore # TODO dont ignore @UnresolvedImport
        svg = etree.fromstring('<r xmlns="http://www.w3.org/2000/svg">{}</r>'.format(svgxml))
        svg = svg[0]
        defs = etree.fromstring('<r xmlns="http://www.w3.org/2000/svg">{}</r>'.format(defsxml))
        defs = defs[1][0]
        #logging.info("SVG-XML %s", utils.dump_xml(svg, depth=4))
        #logging.info("Defs-XML %s", utils.dump_xml(defs, depth=0))
        
        # Remove all unneeded definitions.
        xlinks = set()
        for e in svg.iter():
            xlinks.add(e.get('{http://www.w3.org/1999/xlink}href'))
        xlinks.remove(None)
        for d in defs:
            if not '#'+d.get("id") in xlinks: 
                #logging.info('removing <defs id={}>'.format(d.get("id")))
                defs.remove(d)

        # Fix toplevel SVG-tag attributes
        svg.attrib.pop('style', None)
        width = svg.attrib['width']
        assert width.endswith('ex')
        scale = 0.4
        svg.attrib['width'] = str(float(width[:-2]) * scale) + 'cm'
        height = svg.attrib['height']
        assert height.endswith('ex')
        svg.attrib['height'] = str(float(height[:-2]) * scale) + 'cm'
        
        svg.insert(0, defs)
        assert svg.tag == '{http://www.w3.org/2000/svg}svg', 'svg.tag is {}'.format(svg.tag)
        assert defs.tag == '{http://www.w3.org/2000/svg}defs', "defs.tag is {}".format(defs.tag)
        self._callback(svg)
        self.busy = False
        self.next_in_queue()

    @pyqtSlot(result=str)
    def _get_mml_for_js(self):
        return self._mml_for_js

    @pyqtSlot(str)
    def render_svg(self, mml, callback):
        """Translates the Presentation MathML mml into SVG. Callback is called as callback(svg) where 
        svg is an lxml.etree._Element representing the SVG."""
        if self.error: callback(None); return
        self.queue.append((mml, callback))
        self.next_in_queue()

    @classmethod
    def singleton(_cls):
        global singleton_SVGConverter
        if singleton_SVGConverter is None:
            logging.info("Creating singleton")
            singleton_SVGConverter = SVGConverter()
        return singleton_SVGConverter


class Formula():
    def __init__(self, math, format=None):  # @ReservedAssignment
        self.svg = None
        self.pmathml = None
        if format is None:
            self.cmathml = math
        elif format == 'tex':
            self.cmathml = call_converter("{}2cmml".format(format), math)
        else:
            self.cmathml = call_converter("{}2cmml".format(format), math)
    
    def get_pmathml(self):
        if self.pmathml is not None: return self.pmathml
        self.pmathml = call_converter("cmml2pmml", self.cmathml)
        return self.pmathml

    def get_cmathml(self):
        return self.cmathml

    def get_svg(self, callback):
        if self.svg is not None: 
            callback(self.svg)
            return
        logging.info('get_svg')
        def cb(svg):
            self.svg = svg
            callback(svg)
        SVGConverter.singleton().render_svg(self.get_pmathml(), cb);
        
    
