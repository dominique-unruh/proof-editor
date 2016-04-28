import sys, os
from PyQt5.QtDesigner import QPyDesignerCustomWidgetPlugin

# Needed to make qtdesigner find imported modules
basepath = os.path.normpath(os.path.join(os.path.dirname(__file__),'..'))
sys.path.append(basepath)

from graph.graphview import GraphView

#default_properties = """
#<widget class="MathView" name="mathView">
#</widget>"""
#        return '<widget class="MathView" name="mathView">\n' \
#               ' <property name="toolTip" >\n' \
#               '  <string>PyQt demonstration widget</string>\n' \
#               ' </property>\n' \
#               ' <property name="whatsThis" >\n' \
#               '  <string>PyDemo is a demonstration custom widget written ' \
#               'in Python using PyQt.</string>\n' \
#               ' </property>\n' \
#               '</widget>\n'


class MathViewPlugin(QPyDesignerCustomWidgetPlugin):
    def __init__(self, parent=None):
        super(MathViewPlugin, self).__init__(parent)
        self._initialized = False

    def initialize(self, formEditor):
        if self._initialized: return
        self._initialized = True

    def isInitialized(self):
        return self._initialized

    def createWidget(self, parent):  return GraphView(parent)

    # Return the name of the class that implements the custom widget.
    def name(self): return "GraphView"

    def group(self):
        return "Display Widgets"

    # Return the icon used to represent the custom widget in Designer's widget
    # box.
    # def icon(self):
    # return QIcon(_logo_pixmap)

    # Return a short description of the custom widget used by Designer in a
    # tool tip.
    def toolTip(self):
        return "Interactive Graph viewer"

    # Return a full description of the custom widget used by Designer in
    # "What's This?" help for the widget.
    def whatsThis(self):
        return "Renders Content MathML"

    # Return True if the custom widget acts as a container for other widgets.
    def isContainer(self):
        return False

   # def domXml(self):
   #     return default_properties

    # Return the name of the module containing the class that implements the
    # custom widget.  It may include a module path.
    def includeFile(self):
        return "graph.graphview"

