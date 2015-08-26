from PyQt5.QtCore import QAbstractListModel, QVariant, QModelIndex
from PyQt5.QtCore import Qt
from PyQt5.Qt import QMessageBox
from abc import ABCMeta, abstractmethod
from mathview import Formula, call_converter, ConverterError, mathedhaskell
from typing import List


class Transformation(metaclass=ABCMeta):
    def __init__(self, name, short_descr, argnum=1):
        self.name = name
        #self.command = command
        self.argnum = argnum
        self.short_descr = short_descr

    @abstractmethod
    def transform(self, arguments:List[Formula]) -> Formula: pass


class OCamlTransformation(Transformation):
    def __init__(self, name, command, short_descr, argnum=1):
        super().__init__(name,short_descr,argnum)
        self.command = command
        
    def transform(self, formulas:List[Formula], paths:List[str])->Formula:
        mml = [a.get_cmathml() for a in formulas]
        args = [a for aa in zip(mml, paths) for a in aa] # Interleave lists
        try: res = call_converter('transformation', self.command, *args)
        except ConverterError as e: 
            QMessageBox.warning(self, 'Transformation "{}" failed'.format(self.name), e.errors); return
        #logging.debug("Arguments: "+str(self.arguments))
        return Formula(res)

class HaskellTransformation(Transformation):
    def __init__(self, name, command, short_descr, argnum=1):
        super().__init__(name,short_descr,argnum)
        self.command = command
        
    def transform(self, formulas:List[Formula], paths:List[str])->Formula:
        mml = [a.get_cmathml() for a in formulas]
        args = [a for aa in zip(mml, paths) for a in aa] # Interleave lists
        res = call_converter('transform', self.command, *args)
        return Formula(res)



class TransformationStore(QAbstractListModel):
    def __init__(self, trafos):
        super().__init__()
        self.transformations = trafos

    def rowCount(self, parent:QModelIndex=None) -> int:
        return len(self.transformations)
        
    def data(self, index, role=Qt.DisplayRole) -> QVariant:
        if role == Qt.DisplayRole:
            try: return QVariant(self.transformations[index.row()].name)
            except KeyError: return QVariant()
        
        return QVariant()
        
    def get_trafo(self, index:QModelIndex) -> Transformation:
        return self.transformations[index.row()]

example_transformations = TransformationStore([
    HaskellTransformation("Commutativity","commutativity", "Swap operands"), 
    HaskellTransformation("Associativity","associativity",  "Reorder parentheses"),
    HaskellTransformation("Modus Ponens","modusponens", "A=>B and A gives B", argnum=2), 
    HaskellTransformation("Substitution", "substitution", "Substitute a subterm by another (A(x) and x=y gives A(y))", argnum=2), 
    HaskellTransformation("Calculate", "compute", "Evaluate simple numerical subterms")
    #OCamlTransformation("Modus Ponens - OLD","modusponens", "A=>B and A gives B", argnum=2), 
    #OCamlTransformation("Commutativity","commute", "Swap operands"), 
    #OCamlTransformation("Associativity","associativity",  "Reorder parentheses"),
    #OCamlTransformation("Substitution", "substitute", "Substitute a subterm by another (A(x) and x=y gives A(y))", argnum=2), 
    #OCamlTransformation("Calculate", "calculate", "Evaluate simple numerical subterms")
])
