# -*- coding: utf-8 -*-


from PyQt5.QtCore import pyqtSlot, QItemSelection
from PyQt5.QtWidgets import QMainWindow, QMessageBox, QLineEdit, QListView, QSplitter, QLabel
from PyQt5.Qt import QItemSelectionModel, QPointF, QSortFilterProxyModel
from PyQt5 import Qt
from typing import Optional, List
import logging

# Own modules
from graph.graphview import Node, Edge
from .Ui_mainwin import Ui_MainWindow
import transform
from mathview import Formula, call_converter, ConverterError
from mathview.widgets import MathGraphicsItem, MathView


#mathjax_page = """
#<html>
#<meta charset="utf-8" /> 
#<head>
#<script src='qrc:///testui/mathview.js'></script>
#<script src='https://cdn.mathjax.org/mathjax/latest/MathJax.js'></script>
#</head>
#<body onload="onLoad()">
#<p style="text-align: center">
#<span id="formula-span">
#<math xmlns="http://www.w3.org/1998/Math/MathML" display="{display}">
#{math}
#</math>
#</span>
#</p>
#<span id="selection-rect"></span>
#</body>
#</html>
#"""

class MainWin(QMainWindow, Ui_MainWindow):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        super(MainWin, self).__init__(parent)
        
        QLineEdit,  QListView,  MathView, QSplitter, QLabel # avoid warning about unused imports
        # For MyPy; declares types of variables used below
        self.formula = None # type: QLineEdit
        self.lstTransformations = None # type: QListView
        self.spltTrafoInputs = None # type QSplitter
        self.lblShortTrafoDescr = None # type QLabel
        
        self.setupUi(self)
        
        self.mathviewers = []
        self.arguments = []
        self.current_argnum = 0
        self.current_mathviewer = None
        self.create_mathviewers(1)
        self.select_mathviewer(0)
        
        self.graph.scene().selectionChanged.connect(self.selection_changed)
        
        self.splitter.setSizes([2000, 1000, 500])

        self.trafo_model = transform.example_transformations
        self.proxy_model = QSortFilterProxyModel(self)
        self.proxy_model.setSourceModel(self.trafo_model)
        #self.proxy_model.sort(0)
        self.proxy_model.setFilterCaseSensitivity(Qt.Qt.CaseInsensitive)
        self.lstTransformations.setModel(self.proxy_model)
        self.lstTransformations.selectionChanged = self.trafo_selected
        self.lstTransformations.selectionModel().select(
            self.proxy_model.index(0, 0), QItemSelectionModel.ClearAndSelect)

        self.filter.textChanged.connect(lambda txt: self.proxy_model.setFilterFixedString(txt))

        self.addExamples()
        
        
    def select_mathviewer(self, idx:int) -> None:
        if self.current_mathviewer == idx: return
        if self.current_mathviewer is not None: self.mathviewers[self.current_mathviewer].highlight(False)
        self.current_mathviewer = idx
        logging.debug(str((idx, self.mathviewers[idx])))
        self.mathviewers[idx].highlight()

    @pyqtSlot(QItemSelection, QItemSelection)
    def trafo_selected(self, selected:QItemSelection, deselected:QItemSelection):
        selected = [self.proxy_model.mapToSource(i) for i in selected.indexes()]
        if not selected: 
            self.selected_trafo = None
            # Remove inputs?
            # Or disallow deselection altogher? TODO
            return
        assert len(selected)==1
        trafo = self.trafo_model.get_trafo(selected[0])
        self.selected_trafo = trafo
        self.lblShortTrafoDescr.setText(trafo.short_descr)
        
        argnum = trafo.argnum
        if self.current_argnum != argnum:
            self.create_mathviewers(argnum)
            for w in self.mathviewers[argnum:]:
                w.hide()
            for w in self.mathviewers[:argnum]:
                w.show()
            logging.info(self.spltTrafoInputs.children())
            self.current_argnum = argnum
            
        if self.current_mathviewer >= argnum:
            self.select_mathviewer(0)


    
    def create_mathviewers(self, num:int) -> None:
        while len(self.mathviewers)<num:
            idx = len(self.mathviewers)
            mv = MathView()
            self.mathviewers.append(mv)
            self.arguments.append(None)
            self.spltTrafoInputs.addWidget(mv)
            mv.mouse_press.connect(lambda: (self.select_mathviewer(idx), self.deselect_in_graph()))
            mv.empty_html = """[Please select some math.]
           <div style="width:1cm; height:1cm; background-color:blue"></div>
           <div style="width:4.2ex; height:1ex; background-color:red"></div>
           <span style="font-size: 1cm">
           <div style="width:1cm; height:1cm; background-color:blue"></div>
           <div style="width:1.96ex; height:1ex; background-color:red"></div>
           </span>
           """ # TODO remove

    def formula_node_into_mathview(self, node, i:Optional[int]=None):
        if i is None: i = self.current_mathviewer
        self.mathviewers[i].set_formula(node.graphics().get_formula())
        self.arguments[i] = node
        
    def clear_mathviewers(self):
        for i in range(len(self.mathviewers)):
            self.mathviewers[i].set_formula(None)
            self.arguments[i] = None
            self.select_mathviewer(0)
        
    def addExamples(self):
        formulas = []
        for i in range(10):
            formulas.append(self.addFormulaText("example"+str(i+1)))
        logging.debug(formulas)
        self.clear_mathviewers()
        self.graph.clearSelection()
        #self.formula_node_into_mathview(formulas[1], 0)
        #self.formula_node_into_mathview(formulas[0], 1)
#        self.mathviewers[0].set_formula(formulas[1].graphics().get_formula())
#        self.mathviewers[1].set_formula(formulas[0].graphics().get_formula())

    def get_current_mathview(self) -> MathView:
        return self.mathviewers[self.current_mathviewer]

    def selection_changed(self):
        """Called when the selection in the graph changes"""
        sel = self.graph.selected_items()
        if not sel: return
        if len(sel) != 1: return

        node = sel[0]
        self.formula_node_into_mathview(node)
        #self.get_current_mathview().set_formula(node.graphics().get_formula())
        #if self.current_mathviewer<self.current_argnum-1:
        self.select_mathviewer((self.current_mathviewer+1) % self.current_argnum)


    def addFormulaText(self,  mml:str, sources:List[Formula]=[], format:str='auto') -> Formula:  # @ReservedAssignment
        """Parses and adds a formula to the graph and selects it"""
        formula = Formula(mml,format=format)
        self.addFormula(formula, sources=sources)
        
    def addFormula(self, formula, sources=[]):
        """Parses and adds a formula to the graph and selects it"""
        assert formula is not None
        try: it = MathGraphicsItem(formula)
        except ValueError as e: QMessageBox.warning(None, "Cannot add", str(e)); return

        node = Node(it)
        self.graph.addNode(node)
    
        if not sources:
            node.setPivot(self.graph.centered_point())
        else:
            x = 0.0; y = -100000000000000.0
            for s in sources:
                p = s.getPivot()
                x += p.x()
                y = max(y, p.y())
            x /= len(sources)
            node.setPivot(QPointF(x, y+100))
        
#        if source is None:
#            it.setPos(self.graph.centered_point())
#        else:
#            pos = source.graphics().pos()
#            it.setPos(pos.x(), pos.y()+100)

        self.select_mathviewer(0)
        node.select()
        node.ensure_visible()
 
        for s in sources:
            self.graph.addEdge(Edge(), s, node)
#        if source is not None:
#            self.graph.addEdge(Edge(), source, node)
        
        return node

    @pyqtSlot()
    def on_formula_returnPressed(self):
        try: self.addFormulaText(self.formula.text(),format='tex')
        except ConverterError as e:
            QMessageBox.warning(self, "Converting formula failed", e.errors)

        
        
    @pyqtSlot()
    def javaScriptWindowObjectCleared(self):
        self.view.page().mainFrame().addToJavaScriptWindowObject("controller", self)
        
#    @pyqtSlot(str) # TODO remove
#    def onMathClick(self, path):
#        print("onMathClick: {}".format(path))
#        if path=="": path = ()
#        else: path = tuple(int(x) for x in path.split("."))
#        clicked = self.om.find(path)
#        while clicked is not None:
#            print(clicked)
#            clicked = clicked.parent
    
    @pyqtSlot(str)
    def onMathSelection(self, path):
        logging.info("onMathSelection: {}".format(path))
        self.math_selection = path

    @pyqtSlot(str)
    def on_mathView_selected(self, path):
        logging.info("Selection: {}".format(path))
    
  
    @pyqtSlot()
    def on_btnDelete_clicked(self):
        selection = self.graph.selected_items()
        for it in selection:
            self.graph.removeNode(it)
    
#     @pyqtSlot()
#     def on_actionLoadFormulas_triggered(self):
#         logging.info("on_actionLoadFormulas_triggered")
#         filename = "test.txt"
#         with open(filename) as f:
#             for formula in f:
#                 self.addFormula(formula.rstrip())
    
    def deselect_in_graph(self):
        self.graph.clearSelection()

    @pyqtSlot()
    def on_btnTrafo_clicked(self) -> None:
        trafo = self.selected_trafo
        if not trafo: QMessageBox.warning(self, "Invalid action","You need to select a transformation first"); return
        argnum = trafo.argnum
        assert self.current_argnum == argnum
        
        for i in range(argnum):
            if self.mathviewers[i].get_formula() is None:
                self.select_mathviewer(i)
                self.deselect_in_graph()
                QMessageBox.warning(self, "Invalid action",
                    "You need to choose a formula first for the {}. argument (selected)".format(i+1))
                return
                
        nodes = self.arguments[:argnum]
        formulas = [n.graphics().formula for n in nodes]
        paths = [m.selected_path if m.selected_path is not None else "-" for m in self.mathviewers[:argnum]]
#        path = self.mathviewers[0].selected_path
#        if path is None: QMessageBox.warning(self, "Invalid action","You select a binary subterm of the formula first"); return
        #args = [a for aa in zip(mml, path) for a in aa] # Interleave lists
        try: newformula = trafo.transform(formulas, paths)
        except ConverterError as e: # TODO: should be "UserError"
            QMessageBox.warning(self, 'Transformation "{}" failed'.format(trafo.name), e.errors)
            return
        self.addFormula(newformula, sources=nodes)
        
#         try: res = call_converter('transformation', trafo.command, *args)
#         except ConverterError as e: 
#             QMessageBox.warning(self, 'Transformation "{}" failed'.format(trafo.name), e.errors); return
#         #logging.debug("Arguments: "+str(self.arguments))
#         self.addFormula(res, sources=nodes)
#         logging.info(res)
