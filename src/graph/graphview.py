from PyQt5.QtWidgets import QWidget,  QGraphicsView, QGraphicsScene, QMenu, \
        QGridLayout, QAction, QGraphicsLineItem, QGraphicsItem, QGraphicsObject
from PyQt5.QtCore import Qt, QLineF, pyqtSignal, QPointF, QObject, QTimer, pyqtSlot
from PyQt5.QtGui import QPen, QPainter, QIcon
import logging, math, random
import sip 
from typing import List, Dict

# Own modules
from .Ui_controls import Ui_controls
import utils

class Controls(QWidget, Ui_controls):
    def __init__(self, parent=None):
        super(Controls, self).__init__(parent)
        self.setupUi(self)

# TODO should be in some general utility module
class QGraphicsItem_ItemChangeAware(QGraphicsItem):
    # TODO deprecated, remove
    def __init__(self):
        super().__init__()

    def change_handler(self, change, value):
        pass
        
    def itemChange(self, change, value):
        result = super().itemChange(change, value) # type: ignore
        self.change_handler(change, value)
        if isinstance(result, QGraphicsItem):
            result = sip.cast(result, QGraphicsItem)
        return result

class Node(QObject):
    def __init__(self, graphics):
        super().__init__()
        assert isinstance(graphics, QGraphicsObject)
        # TODO use change signals from QGraphicsObject
        self._graphics = graphics
        graphics._graph_node = self
        self._graph = None
        self._edges = []
        self._prev_pos = None
        graphics.setFlags(QGraphicsItem.ItemIsMovable | QGraphicsItem.ItemIsSelectable | QGraphicsItem.ItemSendsGeometryChanges)
        
#        def change_handler(change, value):
#            if change == QGraphicsItem.ItemPositionChange:
#                self.position_change.emit(value)
#                for e in self._edges:
#                    e.update()
#    
#        graphics.change_handler = change_handler

        graphics.xChanged.connect(self.xy_change)
        graphics.yChanged.connect(self.xy_change)
        
    @pyqtSlot()
    def xy_change(self):
        pos = self._graphics.pos()
        if pos == self._prev_pos: return
        self._prev_pos = pos
        self.position_change.emit(pos)
        for e in self._edges:
            e.update()

    position_change = pyqtSignal(QPointF, name='position_change')
    
    def edges(self):
        return self._edges
    
    def calculateForces(self, force):
        #TODO: sensible
        neighbors = set((id(self), ))
        
        for e in self.edges():
            neighbors.add(id(e._a)); neighbors.add(id(e._b))
            a = e._a; b = e._b
            d = a.getPivot() - b.getPivot()
            dist = math.sqrt(d.y()**2 + d.x()**2)
            if dist==0: continue  # TODO: Wiggle?
            d /= dist # normalize
          
            # Hooke's law
            attract = (dist - 100) * 0.05
            
            d *= attract
        
            force(a, b, d)
            force(b, a, -d)

        self_pos = self.getPivot()
        for n in self._graph._nodes:
            if id(n) in neighbors: continue
            d = self_pos - n.getPivot()
            dist = math.sqrt(d.y()**2 + d.x()**2)
            dist2 = math.sqrt(d.y()**2 + d.x()**2/4) # Hack to have more space horizontally
            if dist==0: force(self, n, QPointF(random.random(), random.random())); continue # type: ignore   # Wiggle
            d /= dist # normalize
          
            # Square-law repulsion
            attract = - 10000 / dist2**2
            # cut off to avoid explosions
            if attract < -10: attract = -10
          
            # smooth cut-off
            attract += 0.1
            if attract > 0: attract = 0
        
            d *= attract
        
            force(self, n, d)
            force(n, self, -d)


    def select(self):
        self._graph._scene.clearSelection()
        self._graphics.setSelected(True)
        
    def center(self):
        self._graph.view.centerOn(self._graphics)
    
    def getPivot(self) -> QPointF:
        "Returns the point where edges should connect (e.g., center)"
        br = self._graphics.boundingRect()
        pos = self._graphics.pos()
        return pos + br.center()
    def setPivot(self, pos:QPointF) -> None:
        "Moves the pivot to pos"
        br = self._graphics.boundingRect()
        self._graphics.setPos(pos - br.center())
        

    def ensure_visible(self):
        self._graph.view.ensureVisible(self._graphics)
    
    def graphics(self):
        return self._graphics
    
    def pos(self):
        return self._graphics.pos()
    def setPos(self, pos):
        self._graphics.setPos(pos)
    

class Edge(object):
    def __init__(self):
        pass
        
    def create_graphics_item(self, a:Node, b:Node) -> QGraphicsItem:
        """Creates the graphics item visualising this edge."""
        self.graphics = QGraphicsLineItem(QLineF(a.getPivot(), b.getPivot()))
        self.graphics.setZValue(-1)
        self.graphics.setPen(QPen(Qt.darkGray,3)) # type: ignore
        self._a = a
        self._b = b
        return self.graphics

    def get_graphics_item(self):
        return self.graphics

    def update(self):
        self.graphics.setLine(QLineF(self._a.getPivot(), self._b.getPivot()))

class QGraphicsView_mod(QGraphicsView):
    def __init__(self, parent=None):
        super(QGraphicsView_mod, self).__init__(parent)

    def contextMenuEvent(self, event):
        self.customContextMenuRequested.emit(event.globalPos())
    
class GraphView(QWidget):
    def __init__(self, parent=None) -> None:
        super(QWidget, self).__init__(parent)
        layout = QGridLayout()
        self.setLayout(layout)
        layout.setContentsMargins(0, 0, 0, 0)

        self._edges = [] # type: List[Edge]
        self._nodes = [] # type: List[Node]
        self._layouting = utils.WithinBlock(catchall=False)

        self.view = QGraphicsView_mod(parent=self)
        self.view.setRenderHint(QPainter.Antialiasing)
        self.view.setScene(QGraphicsScene())
        layout.addWidget(self.view, 0, 0)
        self.scroll_hand_mode()
        
        self.zoomInAction = QAction(QIcon.fromTheme("list-add"), "Zoom In", self)
        self.zoomInAction.triggered.connect(self.zoomIn)
        
        self.zoomOutAction = QAction(QIcon.fromTheme("list-remove"), "Zoom Out", self)
        self.zoomOutAction.triggered.connect(self.zoomOut)
        
        self.scroll_hand_mode_action = QAction("Scroll mode",self)
        self.scroll_hand_mode_action.triggered.connect(self.scroll_hand_mode)
        self.selection_mode_action = QAction("Selection mode",self)
        self.selection_mode_action.triggered.connect(self.selection_mode)
    
        self.view.contextMenuPolicy = Qt.NoContextMenu # type: ignore
        self.view.customContextMenuRequested.connect(self.show_context_menu) # type: ignore
        
        self.controls = Controls(parent=self)
        layout.addWidget(self.controls, 0, 0, Qt.AlignTop | Qt.AlignLeft) # type: ignore
        
        self.controls.btnZoomIn.clicked.connect(self.zoomIn) # type: ignore
        self.controls.btnZoomOut.clicked.connect(self.zoomOut) # type: ignore

        self.timer = QTimer(self)
        self.timer.timeout.connect(self.graph_layout_step)
        self.start_layout()
        
        self._scene = self.view.scene()

    def selection_mode(self):
         self.view.setDragMode(QGraphicsView.RubberBandDrag)
    
    def scroll_hand_mode(self):
         self.view.setDragMode(QGraphicsView.ScrollHandDrag)
         
    def show_context_menu(self, pos):
        menu = QMenu(self)
        for a in [self.zoomInAction, self.zoomOutAction, None,  self.scroll_hand_mode_action, self.selection_mode_action]:
            if a is None: menu.addSeparator()
            else: menu.addAction(a)
        menu.exec(pos)
        
    def scene(self):
        return self._scene
        
    zoom_factor = 1.5
    
    def zoomOut(self):
        self.view.scale(1/self.zoom_factor, 1/self.zoom_factor)
    
    def zoomIn(self):
        self.view.scale(self.zoom_factor, self.zoom_factor)
    
    def addNode(self, node):
        logging.debug("ADDING")
        assert node._graph is None
        node._graph = self
        self._nodes.append(node)
        self._scene.addItem(node._graphics)
        node.position_change.connect(self.node_position_change)
        self.start_layout()

    def clearSelection(self):
        self._scene.clearSelection()

    def removeNode(self, node):
        assert node._graph is self
        logging.debug("Remove node "+str(node))
        #for e in list(node._edges): # Copy list because 
        while node._edges: # Don't use iterator, because removeEdge mutates the edge list
            e = node._edges[0]
            logging.debug("Remove edge: "+str(e))
            self.removeEdge(e)
        self._nodes.remove(node)
        self._scene.removeItem(node._graphics)
        node._graph = None
        
    def removeEdge(self, edge):
        if edge._graph is None: return
        assert edge._graph is self
        self._edges.remove(edge)
        self._scene.removeItem(edge.get_graphics_item())
        edge._graph = None
        logging.debug("PRE-REMOVE {} {} {} {} {}".format(edge, edge._a, edge._b, edge._a._edges, edge._b._edges))
        try: edge._a._edges.remove(edge)
        except ValueError: pass
        try: edge._b._edges.remove(edge)
        except ValueError: pass
        logging.debug("REMOVED {} {} {} {} {}".format(edge, edge._a, edge._b, edge._a._edges, edge._b._edges))

    def addEdge(self, edge, a, b):
        assert isinstance(a, Node)
        assert isinstance(b, Node)
        assert a._graph == self
        assert b._graph == self
        edge._a = a
        edge._b = b
        edge._graph = self
        a._edges.append(edge)
        b._edges.append(edge)
        self._edges.append(edge)
        item = edge.create_graphics_item(a, b)
        self._scene.addItem(item)
        
    def selected_items(self):
        return [item._graph_node for item in self._scene.selectedItems()]

    def graph_layout_step(self) -> None:
        with self._layouting:
            min_nudge = 1
            
            #logging.info("graph_layout_step")
            nudges = dict() # type: Dict[int,QPointF]
            for n in self._nodes:
                nudges[id(n)] = QPointF(0.0, 0.0)
            
            def force(a, b, F): # a issuing object, b object force works on
                if math.isnan(F.x()) or math.isnan(F.y()):
                    logging.warning("Got a NaN as force")
                    return
                    
                nudges[id(b)] += F
            
            for n in self._nodes:
                n.calculateForces(force)
            
            nudged = False
            
            for n in self._nodes:
                nudge = nudges[id(n)]
                if nudge.manhattanLength() < min_nudge: continue
                nudged = True
                # If n is currently being dragged, don't move it
                if self._scene.mouseGrabberItem() is not None \
                    and n._graphics.isSelected(): continue
                pos = n.pos()
                n.setPos(pos + nudges[id(n)])

            if not nudged:
                self.stop_layout()

    def stop_layout(self) -> None:
        logging.info("Graph layout stopped.")
        self.timer.stop()
        
    def start_layout(self) -> None:
        if self.timer.isActive(): return
        logging.info("Graph layout starting.")
        self.timer.start(50)
        
    def node_position_change(self, pos):
        if self._layouting.active: return
        self.start_layout()
        
    def centered_point(self):
        return self.view.mapToScene(
            (self.view.viewport().rect().topLeft() +
            self.view.viewport().rect().bottomRight()) / 2)
