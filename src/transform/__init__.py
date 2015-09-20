from PyQt5.QtCore import QAbstractListModel, QVariant, QModelIndex
from PyQt5.QtCore import Qt
from PyQt5.Qt import QMessageBox
from abc import ABCMeta, abstractmethod
from mathview import Formula, ConverterError, mathedhaskell
from typing import List

import haskell, utils, logging


class Transformation(metaclass=ABCMeta):
    def __init__(self, name, short_descr, argnum=1):
        self.name = name
        #self.command = command
        self.argnum = argnum
        self.short_descr = short_descr

    @abstractmethod
    def transform(self, arguments:List[Formula], paths:List[str]) -> Formula: pass


# class OCamlTransformation(Transformation):
#     def __init__(self, name, command, short_descr, argnum=1):
#         super().__init__(name,short_descr,argnum)
#         self.command = command
#         
#     def transform(self, formulas:List[Formula], paths:List[str])->Formula:
#         mml = [a.get_cmathml() for a in formulas]
#         args = [a for aa in zip(mml, paths) for a in aa] # Interleave lists
#         try: res = call_converter('transformation', self.command, *args)
#         except ConverterError as e: 
#             QMessageBox.warning(None, 'Transformation "{}" failed'.format(self.name), e.error)
#             return None
#         #logging.debug("Arguments: "+str(self.arguments))
#         return Formula(res)



class HaskellTransformation(Transformation):
    def __init__(self, name, function, short_descr, argnum=1):
        super().__init__(name,short_descr,argnum)
        self.function = function

    def transform(self, formulas:List[Formula], paths:List[str])->Formula:
        formulas = [a.get_haskell() for a in formulas]
#         formulas = utils.foldr(haskell.lib.consOpenmath,haskell.lib.nilOpenmath(),formulas)
        
        paths = [haskell.lib.parsePath(p) for p in paths]
#         paths = utils.foldr(haskell.lib.consMaybePath,haskell.lib.nilMaybePath(),paths)
        args = [haskell.lib.pairOpenmathMaybePath(o,p) for o,p in zip(formulas,paths)]
        args = utils.foldr(haskell.lib.consOpenmathMaybePath,haskell.lib.nilOpenmathMaybePath(),args)
#         args = [a for aa in zip(formulas, paths) for a in aa] # Interleave lists
        res = haskell.lib.runExceptTrafo(self.function(args))
        logging.debug(haskell.lib.isError(res))
        if haskell.lib.isError(res):
            raise ConverterError(str(self.function),haskell.lib.userErrorShortDescr(res),haskell.lib.userErrorLongDescr(res))
        return Formula(haskell.lib.errorRight(res))



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
    HaskellTransformation("Commutativity", haskell.lib.commutativity, "Swap operands"), 
    HaskellTransformation("Associativity", haskell.lib.associativity,  "Reorder parentheses"),
    HaskellTransformation("Modus Ponens", haskell.lib.modusPonens, "A=>B and A gives B", argnum=2), 
    HaskellTransformation("Substitution", haskell.lib.substitution, "Substitute a subterm by another (A(x) and x=y gives A(y))", argnum=2), 
    HaskellTransformation("Calculate", haskell.lib.compute, "Evaluate simple numerical subterms")
    #OCamlTransformation("Modus Ponens - OLD","modusponens", "A=>B and A gives B", argnum=2), 
    #OCamlTransformation("Commutativity","commute", "Swap operands"), 
    #OCamlTransformation("Associativity","associativity",  "Reorder parentheses"),
    #OCamlTransformation("Substitution", "substitute", "Substitute a subterm by another (A(x) and x=y gives A(y))", argnum=2), 
    #OCamlTransformation("Calculate", "calculate", "Evaluate simple numerical subterms")
])
