from PyQt5.QtWebKitWidgets import QWebView
from PyQt5.QtWebKit import QWebSettings
from PyQt5.QtWidgets import QWidget,  QGridLayout
from PyQt5.QtCore import pyqtProperty, pyqtSlot, pyqtSignal, QUrl
from PyQt5.Qt import QColor, QBrush,  QMouseEvent
# from PyQt5.QtCore import Qt
from PyQt5 import Qt
from PyQt5.QtSvg import QGraphicsSvgItem
from lxml import etree  # @UnresolvedImport
import logging, sys

# Own modules
import utils
from mathview import Formula, base_url

mathjax_page = """
<html>
<meta charset="utf-8" /> 
<head>
<script src='mathview.js'></script>
<script src='mathjax/MathJax.js'></script>
</head>
<body onload="onLoad()">
<span style="font-size: {size}cm" id="formula-span">
<math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">
{math}</math>
</span>
<span id="selection-rect"></span>
</body>
</html>
"""

class MathGraphicsItem(QGraphicsSvgItem):
    def __init__(self, math:Formula) -> None:
        assert math is not None
        super(MathGraphicsItem, self).__init__(
            utils.file_path('resources/icons/scalable/status/image-loading.svg'))
        
        self.formula = math
        math.get_svg(self.rendered)
        self.bgcolor = QColor(0xf0f0f0)
        self.bgcolor_select = QColor(0xd0d0d0)
        
    def paint(self, painter, option, widget=None):
        painter.setPen(Qt.Qt.NoPen)
        color = self.bgcolor_select if self.isSelected() else self.bgcolor
        painter.setBrush(QBrush(color))
        painter.drawRect(self.boundingRect())
        super().paint(painter, option, widget)
    
    def get_formula(self):
        return self.formula


    def rendered(self, svg):
        if svg is None:
            self.renderer().load("icons/theme/scalable/emblems/emblem-unreadable.svg")
            self.setElementId("")
            return
            
        assert isinstance(svg, etree._Element), type(svg)
        svg = etree.tostring(svg)
        assert isinstance(svg, bytes)
        self.renderer().load(svg)
        self.setElementId("") # Seems only way to trigger recalculation of bounding box

 
# class MathPicture(QObject):
#     """@deprecated"""
#     def __init__(self):
#         super(MathPicture, self).__init__()
#         webpage = QWebPage()
#         webpage.settings().setUserStyleSheetUrl(base_url.resolved(QUrl("mathpicture.css"))) 
#         webpage.mainFrame().javaScriptWindowObjectCleared.connect(self.javaScriptWindowObjectCleared)
#         webpage.mainFrame().setScrollBarPolicy(Qt.Vertical, Qt.ScrollBarAlwaysOff);
#         webpage.mainFrame().setScrollBarPolicy(Qt.Horizontal, Qt.ScrollBarAlwaysOff);
#         self.webpage = webpage
#         self.format = "auto"
#     
#         
#     def close(self):
#         self.webpage.setParent(None)
#         self.webpage.deleteLater()
#   
#     def javaScriptWindowObjectCleared(self):
#         self.webpage.mainFrame().addToJavaScriptWindowObject("controller", self)
#     
#     rendered = pyqtSignal(QImage, arguments=["image"])
#     
#     @pyqtSlot()
#     def onMathRendered(self):
#         frame = self.webpage.mainFrame()
#         size = frame.contentsSize()
#         self.webpage.setViewportSize(QSize(size.width(), size.height()+100)) # +100 to make sure the progress tooltip is not drawn in the image
# 
#         image = QImage(size, QImage.Format_RGB888) 
#         painter = QPainter(image) 
#         frame.render(painter)
#         painter.end()
#         self.rendered.emit(image)
#         
#     def render(self, math):
#         assert isinstance(math, Formula), "isinstance({}, Formula)".format(type(math))
#         html = mathjax_page.format(math.get_pmathml())
#         self.webpage.mainFrame().setHtml(html,base_url)
        
        


class MathView(QWidget):
    class WebView(QWebView):
        def __init__(self, parent:QWidget=None) -> None:
            super().__init__(parent)
#        def mousePressEvent(self, event:QMouseEvent) -> None:
#            super().mousePressEvent(event)
#            logging.info("MOUSE")
        def mousePressEvent(self, event:QMouseEvent) -> None:
            super().mousePressEvent(event)
            self.mouse_press.emit()
        mouse_press = pyqtSignal()

    def __init__(self, parent:QWidget=None) -> None:
        super().__init__(parent)
        
        self.highlit = False
        
        self.webview = MathView.WebView(parent=self)
        self.webview.settings().setUserStyleSheetUrl(base_url.resolved(QUrl("mathview.css"))) 
        self.webview.page().mainFrame().javaScriptWindowObjectCleared.connect(self.javaScriptWindowObjectCleared)
        self.webview.loadFinished.connect(self.load_finished) # type: ignore
        self.webview.mouse_press.connect(self.mouse_press)
        
        layout = QGridLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.webview)
        
        
        self._empty_html = ""
        self.formula = None # type: Formula

    mouse_press = pyqtSignal()

    @pyqtSlot(bool)
    def load_finished(self, ok:bool) -> None:
        self.highlight(self.highlit)
        self.refresh_highlight()
 
    def refresh_highlight(self):
        body = self.webview.page().mainFrame().findFirstElement("body")
        if self.highlit:
            body.addClass("highlight-bg")
        else:
            body.removeClass("highlight-bg")
 
    def highlight(self, highlight=True):
        if self.highlit == highlight: return
        self.highlit = highlight
        self.refresh_highlight()
 
    def javaScriptWindowObjectCleared(self):
        self.webview.page().mainFrame().addToJavaScriptWindowObject("controller", self)
    
    selected = pyqtSignal(str, arguments=['path'])
    rendered = pyqtSignal()

    @pyqtSlot(str)
    def onMathSelection(self, path):
        logging.info("MathView: Selection: {}".format(path))
        self.selected_path = path
        self.selected.emit(path)

    @pyqtSlot()
    def onMathRendered(self):
        self.rendered.emit()
    
    @pyqtProperty(str) # type: ignore
    def empty_html(self):
        return self._empty_html
    @empty_html.setter # type: ignore
    def empty_html(self, html):
        self._empty_html = html
        if not self.formula:
            self.webview.setHtml(self._empty_html)
 
    @empty_html.reset # type: ignore
    def empty_html(self):
        self._empty_html = ""
        if not self.formula:
            self.webview.setHtml(self._empty_html)

    def get_formula(self):
        return self.formula

    # Under Windows, the ex-value is ~2.26 smaller than in Linux, so we add an extra zoom factor for the font size
    extra_zoom = 2.26 if sys.platform=="win32" else 1

    def set_formula(self, math):
        self.formula = math
        self.selected_path = None
        
        if math is None:
            self.webview.setHtml(self._empty_html)
            return
            
        try: html = mathjax_page.format(math=math.get_pmathml(),size=0.75*self.extra_zoom)
        except ValueError as e: html = '[{}]'.format(str(e))
        
        self.webview.settings().setAttribute(QWebSettings.LocalContentCanAccessRemoteUrls, True)
        self.webview.setHtml(html,base_url)
