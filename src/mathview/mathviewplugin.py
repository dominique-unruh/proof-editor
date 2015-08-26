import sys, os
from PyQt5.QtDesigner import QPyDesignerCustomWidgetPlugin

# Needed to make qtdesigner find imported modules
basepath = os.path.normpath(os.path.join(os.path.dirname(__file__),'..'))
sys.path.append(basepath)

from mathview.widgets import MathView


class MathViewPlugin(QPyDesignerCustomWidgetPlugin):
    def __init__(self, parent=None):
        super(MathViewPlugin, self).__init__(parent)

        self._initialized = False

    def initialize(self, formEditor):
        if self._initialized:
            return

        self._initialized = True

    def isInitialized(self):
        return self._initialized

    def createWidget(self, parent):
        return MathView(parent)

    def name(self):
        return "MathView"

    def group(self):
        return "Display Widgets"

    def toolTip(self):
        return "Content MathML viewer"

    def whatsThis(self):
        return "Renders Content MathML (by using MathJax in a QWebView)"

    def isContainer(self):
        return False

#    def domXml(self):
#        return default_properties
        
    def includeFile(self):
        return "mathview.widgets"

