import PyQt5, PyQt5.Qt, PyQt5.QtCore, PyQt5.QtGui, PyQt5.QtWidgets, PyQt5.QtSvg, PyQt5.QtWebKit, PyQt5.QtWebKitWidgets, PyQt5.QtPrintSupport, PyQt5.QtNetwork, PyQt5.QtDBus, PyQt5.QtDesigner, PyQt5.QtGui, PyQt5.QtHelp # type: ignore

import os, subprocess, sys, re, builtins, typing
import sip, inspect # type: ignore
from typing import List, Dict, Any, Set, IO, Iterable, Tuple, Union


Type = Union[str,Tuple[str,Union[Tuple[Any],Tuple[Any,Any]]]]

modules = [
    PyQt5, PyQt5.QtCore, PyQt5.QtGui, PyQt5.QtWidgets, PyQt5.QtSvg, PyQt5.QtWebKit, 
    PyQt5.QtWebKitWidgets, PyQt5.QtPrintSupport, PyQt5.QtNetwork, PyQt5.QtDBus, 
    PyQt5.QtDesigner, PyQt5.QtGui, PyQt5.QtHelp,
    PyQt5.Qt, # Last!
] # type: List[module]

objects = {} # type: Dict[str,Dict[str,Any]]

knownobjects = {} # type: Dict[int,str]


exceptions = {

    # 'PyQt5.QtGui.QPainterPath.toFillPolygon':
    # {'kind':'literal',
    #  'code': '# PyQt5.QtGui.QPainterPath.toFillPolygon skipped to break circulare references'},
    # 'PyQt5.QtGui.QPainterPath.toFillPolygons':
    # {'kind':'literal',
    #  'code': '# PyQt5.QtGui.QPainterPath.toFillPolygons skipped to break circulare references'},
    # 'PyQt5.QtGui.QPainterPath.toSubpathPolygons':
    # {'kind':'literal',
    #  'code': '# PyQt5.QtGui.QPainterPath.toSubpathPolygons skipped to break circulare references'},

#     'PyQt5.QtCore.QVariant': # TODO remove
#     {'kind':'literal',
#      'code':
# """class QVariant():
#     def __init__(self,x:typing.Any=None) -> None:
#         pass"""},

    'PyQt5.QtCore.pyqtSignal':
    {'kind':'literal',
     'code':
"""class pyqtSignal(object):
    def __init__(self, *types:typing.Union[type,str], name:str=None, 
                 revision:int=0, arguments:typing.List[str]) -> None: pass
    def connect(self, slot:typing.Any, type:typing.Any=None, 
                no_receiver_check:bool=False) -> None: pass"""},

    'PyQt5.QtCore.pyqtSlot':
    {'kind':'literal',
     'code':
"""def pyqtSlot(*types:typing.Union[type,str], name:str=None, revision:int=0, 
             result:typing.Union[type,str]) -> typing.Callable[[typing.Any],typing.Any]: 
    pass"""},


'PyQt5.QtCore.pyqtProperty':
    {'kind':'literal',
     'code':
"""def pyqtProperty(type:type, fget:typing.Callable[[],typing.Any]=None, fset:typing.Callable[[],None]=None,
                 freset:typing.Callable[[],None]=None, fdel:typing.Callable[[],None]=None, doc:str=None,
                 designable:bool=True, scriptable:bool=True, stored:bool=True,
                 user:bool=False, constant:bool=False, final:bool=False, 
                 notify:pyqtSignal=None, revision:int=0) -> object:
    pass"""},
} # type: Dict[str,Dict[str,Any]]

def skip(name:str,reason:str) -> None:
    exceptions[name] = {
        'kind':'literal',
        'code':"# "+name,
        'errors':["skipped manually. Reason: {reason} issues".format(name=name,reason=reason)]
    }

skip('PyQt5.QtCore.QTemporaryFile.open','overloading')
skip('PyQt5.QtCore.QAbstractTableModel.parent','overloading')
skip('PyQt5.QtCore.QAbstractListModel.parent','overloading')
skip('PyQt5.QtCore.QAbstractProxyModel.headerData','overloading')
skip('PyQt5.QtCore.QStringListModel.data','overloading')
skip('PyQt5.QtCore.QIdentityProxyModel.parent','overloading')
skip('PyQt5.QtCore.QIdentityProxyModel.headerData','overloading')
skip('PyQt5.QtNetwork.QSslSocket.connectToHost','overloading')
skip('PyQt5.QtGui.QImage.devicePixelRatio','overloading')
skip('PyQt5.QtGui.QTextTable.setFormat','overloading')
skip('PyQt5.QtGui.QPixmap.devicePixelRatio','overloading')
skip('PyQt5.QtGui.QBitmap.transformed','overloading')
skip('PyQt5.QtGui.QBitmap.swap','overloading')
skip('PyQt5.QtGui.QStandardItemModel.itemData','overloading')
skip('PyQt5.QtGui.QStandardItemModel.setItemData','overloading')
skip('PyQt5.QtGui.QWindow.setParent','overloading')
skip('PyQt5.QtGui.QTextList.setFormat','overloading')
skip('PyQt5.QtWidgets.QWidget.setParent','overloading')
skip('PyQt5.QtWidgets.QAbstractItemView.update','overloading')
skip('PyQt5.QtWidgets.QListWidget.openPersistentEditor','overloading')
skip('PyQt5.QtWidgets.QListWidget.closePersistentEditor','overloading')
skip('PyQt5.QtWidgets.QGraphicsView.render','overloading')
skip('PyQt5.QtWidgets.QApplication.topLevelAt','overloading')
skip('PyQt5.QtWidgets.QTreeWidget.openPersistentEditor','overloading')
skip('PyQt5.QtWidgets.QTreeWidget.closePersistentEditor','overloading')
skip('PyQt5.QtWidgets.QRubberBand.resize','overloading')
skip('PyQt5.QtWidgets.QGraphicsProxyWidget.paint','overloading')
skip('PyQt5.QtWidgets.QGraphicsProxyWidget.setGeometry','overloading')
skip('PyQt5.QtWidgets.QHeaderView.scrollTo','overloading')
skip('PyQt5.QtWidgets.QTextEdit.find','overloading')
skip('PyQt5.QtWidgets.QGraphicsSimpleTextItem.paint','overloading')
skip('PyQt5.QtWidgets.QStylePainter.begin','overloading')
skip('PyQt5.QtWidgets.QStackedLayout.addWidget','overloading')
skip('PyQt5.QtWidgets.QGraphicsPixmapItem.paint','overloading')
skip('PyQt5.QtWidgets.QFileSystemModel.parent','overloading')
skip('PyQt5.QtWidgets.QSplashScreen.repaint','overloading')
skip('PyQt5.QtWidgets.QGraphicsTextItem.paint','overloading')
skip('PyQt5.QtWidgets.QPlainTextEdit.find','overloading')
skip('PyQt5.QtWidgets.QTableWidget.openPersistentEditor','overloading')
skip('PyQt5.QtWidgets.QTableWidget.closePersistentEditor','overloading')
skip('PyQt5.QtWebKitWidgets.QGraphicsWebView.sizeHint','overloading')
skip('PyQt5.QtWebKitWidgets.QGraphicsWebView.setGeometry','overloading')
skip('PyQt5.QtHelp.QHelpContentModel.data','overloading')
skip('PyQt5.QtHelp.QHelpContentModel.parent','overloading')
skip('PyQt5.QtHelp.QHelpIndexModel.createIndex','overloading')
skip('PyQt5.QtDesigner.QDesignerFormWindowInterface.cursor','overloading')
skip('PyQt5.QtDesigner.QDesignerPropertySheetExtension.setProperty','multiple inheritance')
skip('PyQt5.QtDesigner.QDesignerPropertySheetExtension.property','multiple inheritance')






#skip('PyQt5.Qt.','overloading')




def obj_name(obj:object) -> Union[str]:
    global knownobjects
    name = knownobjects.get(id(obj))
    if name: 
        assert eval(name) is obj
        return name

    try:
        name = obj.__module__+"."+obj.__name__ #type:ignore
        if eval(name) is obj: return name
    except: pass

    try:
        name = obj.__module__+"."+obj.__qualname__ #type:ignore
        if eval(name) is obj: return name
    except: pass

    return "#{}-{}".format(id(obj),name)

def find_full_type_name(typ:type,class_hint:str,name_hint:str) -> str:
    try:
        name = typ.__module__+'.'+typ.__name__
        t = eval(name)
        if t is typ: return name
    except: pass

    try:
        name = class_hint+'.'+typ.__name__
        t = eval(name)
        if t is typ: return name
    except: pass

    assert False, (class_hint,typ.__name__,typ,name)

def find_full_object_name(obj:object,name_hint:str=""):
    try:
        module = inspect.getmodule(obj).__name__
    except: pass

    try:
        name = module + '.' + obj.__name__ #type:ignore
        obj2 = eval(name)
        if obj is obj2: return name
    except: pass

    try:
        name = obj.__name__ #type:ignore
        obj2 = eval(name)
        if obj is obj2: return name
    except: pass

    try:
        name_hint2 = name_hint[name_hint.rindex('.')+1:]
        print(name_hint2)
        name = module + '.' + name_hint
        obj2 = eval(name)
        print(name)
        if obj is obj2: return name
    except: pass

    print("Tried to find object",obj,name_hint,module)
    assert False



elementary_types = {
    'str': 'str', 'int':'int', 'bytes':'bytes', 'bool':'bool', 'float':'float',
    'sip.voidptr': 'sip.voidptr',
    'QStringRef': 'str', 'QString':'str',
    'object':'object', 
    'callable': 'typing.Any', # TODO: 'typing.Callable' when supported
    'signal': 'typing.Any', # TODO
}
def convert_type(typ:str) -> Type:
    if typ is None: return 'None'
    if typ in elementary_types:
        return elementary_types[typ]
    if typ.startswith("list-of-"):
        typ = typ[len("list-of-"):]
        return "typing.List",(convert_type(typ),)
    if typ.startswith("dict-of-"):
        match = re.fullmatch("dict-of-(.*)-(.*)",typ) # type:ignore
        return "typing.Dict",(convert_type(match.group(1)),convert_type(match.group(2)))
    if typ.startswith("tuple-of-"):
        match = re.fullmatch("tuple-of-(.*)-(.*)",typ) # type:ignore
        return "typing.Tuple",(convert_type(match.group(1)), convert_type(match.group(2)))
    for m in modules:
        if m is PyQt5.Qt: continue
        try:
            eval(m.__name__+"."+typ)
            return m.__name__+"."+typ
        except: pass
    raise SyntaxError("Could not parse type {}".format(typ))

python_keywords = set(['from']) # type: Set[str]
def parse_docstring(doc:str):
    m = re.fullmatch(r"[a-zA-Z0-9_.]+\(([^>]*)\)(?: -> (.*))?",doc) # type:ignore
    if m is None: raise SyntaxError("Could not parse docstring signature '{}'".format(doc))
    args = m.group(1)
    ret = convert_type(m.group(2))
    args = args.split(',')
    parsed_args = [] # type: List[Tuple[str,Type,bool]]
    i=0
    for a in args:
        if a=='': continue
        m = re.fullmatch(r"\s*([a-zA-Z0-9._-]+)(?:\s+([a-zA-Z0-9_]+)(=.*)?)?\s*",a)# type:ignore
        if m is None:
            raise SyntaxError("cannot convert docstring (parsing arg '{}')".format(a))
        atyp = convert_type(typ=m.group(1))
        aname = m.group(2)
        if aname is None: i+=1; aname = "x__"+str(i)
        if aname in python_keywords: aname += "_"
        adefault = m.group(3) is not None
        parsed_args.append((aname,atyp,adefault))
    return {'return':ret, 'args':parsed_args}



fun_classes = ('builtin_function_or_method','methoddescriptor')
class_classes = ('pyqtWrapperType','wrappertype','enumtype','simplewrapper')

def scan_object(name:str,clazz:str="") -> None:
    errors = [] #type: List[str]
    try:
        obj = eval(name)
    except Exception as e:
        obj = None
        errors.append("Could not access {}: {}".format(name,e))

    if obj is not None:
        typ = type(obj).__name__
    else:
        typ = None
    try: module = inspect.getmodule(obj).__name__
    except: module = None

    # Check if it is a copy
    copy = (typ in fun_classes or typ in class_classes) \
           and module is not None \
           and not (name==module or name.startswith(module+"."))

    if not copy and obj is not None:
        knownobjects[id(obj)] = name

    info = None # type: Dict[str,Any]

    if name in exceptions:
        info = exceptions[name]

    elif copy:
        #print("COPY?",module,name)
        original = find_full_object_name(obj,name_hint=name)
        info = { 'kind': 'copy',
                 'original': original,
                 'depends': [original] }

    elif typ is None:
        info = { 'depends': [],
                 'kind': 'unknown' }

    elif typ in fun_classes:
        doc = None # type: str
        try: 
            doc = eval(name+".__doc__")
            assert doc
        except: errors.append("Cannot access docstring")
        
        signatures = [] # type:List[Any]
        if doc is not None:
            doclines = doc.split("\n")
            assert doclines
            for l in doclines:
                try: 
                    signature = parse_docstring(l)
                    signatures.append(signature)
                except SyntaxError as e:
                    errors.append(str(e))

        depends = [] # type: List[str]
        for sig in signatures:
            depends.append(sig['return'])
            for a,t,d in sig['args']:
                depends.append(t)

        info = { 'kind': 'function',
                 'soft-depends': depends,
                 #'depends': depends, #TODO?
                 'signatures': signatures }

    elif typ in class_classes:
        bases = [obj_name(b) for b in obj.__bases__
                 if b not in (sip.wrapper, sip.simplewrapper)]
        depends = bases
        children = [name+"."+n 
                    for n in obj.__dict__.keys() 
                    if not n.startswith("_")]

        init_errors = [] # type: List[str]
        init_doc = None # type: str
        try: 
            init_doc = eval(name+".__doc__")
            assert doc
        except: init_errors.append("Cannot access docstring")
        
        init_signatures = [] # type:List[Any]
        if init_doc is not None:
            doclines = init_doc.split("\n")
            assert doclines
            for l in doclines:
                try: 
                    signature = parse_docstring(l)
                    init_signatures.append(signature)
                except SyntaxError as e:
                    init_errors.append(str(e))

        init_depends = [] # type: List[str]
        for sig in init_signatures:
            for a,t,d in sig['args']:
                init_depends.append(t)

        init_name = name+".__init__"
        init = ({'kind':'function',
                 'soft-depends':'init_depends',
                 'depends':[],
                 'errors':init_errors,
                 'name':init_name,
                 'signatures':init_signatures})
        objects[init_name] = init #type:ignore

        for b in bases: assert b is not None

        for c in children: scan_object(c,clazz=name)
        children.append(init_name)

        info = { 'depends': depends,
                 'kind': 'class',
                 'bases': bases,
                 'children': children }

    elif typ == 'module':
        children = [name+"."+n 
                    for n in obj.__dict__.keys() 
                    if not n.startswith("_")]

        for c in children: scan_object(c)

        info = { 'kind': 'module',
                 'children': children }

    else:
        fulltype = find_full_type_name(typ=type(obj),class_hint=clazz,name_hint=name)
        info = { 'depends': [fulltype],
                 'kind': 'value',
                 'type': fulltype }

    if 'depends' not in info: info['depends'] = []
    if 'soft-depends' not in info: info['soft-depends'] = []
    info['name'] = name
    info['errors'] = errors
    objects[name] = info


def scan_all() -> None:
    for m in modules:
        scan_object(m.__name__)


def nested_dependencies(depkind:str) -> None:
    depkind_nested = depkind+"-nested"
    def get(name:str) -> List[str]:
        info = objects[name]
        nested = info.get(depkind_nested)
        if nested is not None:
            return nested
        
        deps = info[depkind]
        for d in deps: assert d is not None, info
        nested = set(deps)
        for c in info.get('children',()):
            nested.update(get(c))
        
        info[depkind_nested] = nested
        return nested

    for o in objects:
        get(o)


def sort_children(name:str):
    children = objects[name]['children']
    sorted = [] # type: List[str]
    done = set() #type: Set[str]
    inwork = set() #type: Set[str] # for cycle detection
    def add(c):
        if c in done: return
        if c in inwork: 
            raise RuntimeError("Circular dependency:",c,inwork)
        inwork.add(c)
        #print("CONSIDER",c)
        for d in objects[c]['depends-nested']:
            #print("X",c,objects[c]['depends-nested'],d)
            if c==d: continue
            if d.startswith(c+"."): continue # Dependency of own children
            if d.startswith(name+"."):
                d2 = d
                dot_idx = d2.find('.',len(name)+1)
                if dot_idx!=-1: d2 = d2[:dot_idx]
                #print("In {}: {} -> {}".format(name,d,d2))
                #print("{} -deps- {}".format(c,d2))
                add(d2)
        #print("ADD",c)
        done.add(c)
        sorted.append(c)
    for c in children:
        add(c)
    return sorted

def sort_all_children():
    for o in objects:
        obj = objects[o]
        if 'children' in obj:
            try:
                obj['children-sorted'] = sort_children(o)
            except RuntimeError as e:
                print("Was sorting {}".format(o))
                raise e

# We do not list submodules as children of a module, because they will
# be printed separately
def remove_module_children():
    for o in objects.values():
        if o['kind']!='module': continue
        o['children'] = [c for c in o['children']
                         if objects[c]['kind']!='module']


def truncate_fun_sig_deps(name:str,module:str,signature:Dict,printed:Set[str]) -> Tuple[List[str],Dict]:
    fixed = [] # type: List[str]
    moduledot = module+"."
    modulelen1 = len(module)+1
    def fix(typ:Type):
        if isinstance(typ,str):
            if not typ.startswith(moduledot): return typ
            if typ in printed: return typ
            if typ.find(".",modulelen1)==-1: return typ # forward refs to toplevel class supported
            fixed.append(typ)
            return "typing.Any"
        else:
            a,b = typ
            return a,[fix(t) for t in b]
    signature = {'return': fix(signature['return']),
                 'args': [(a,fix(t),d) for a,t,d in signature['args']]}
    return (fixed,signature)
    


def typ2str(typ:Type) -> str:
    if isinstance(typ,str):
        return typ
    else:
        a,b = typ
        return a+"["+",".join(typ2str(t) for t in b)+"]"

def print_object(file:IO[str],name:str,parent:str=None,module:str=None,
                 indent:str="",ismember:bool=False,
                 have_printed:Set[str]=set()) -> bool:
    obj = objects[name]
    assert name==obj['name']
    kind = obj['kind']
    errors = "".join("{ind}# ERROR: {err}\n".format(ind=indent,err=err)
                     for err in obj['errors'])
    wrote_non_comment = False

    if parent==None:
        shortname = name
    else:
        assert name.startswith(parent+".")
        shortname = name[len(parent)+1:]
        
    assert kind=='module' or shortname.find(".")==-1, (parent,name,shortname)
    assert kind=='module' or module is not None
    assert not (kind=='module' and parent is not None)
    assert not (kind=='module' and module is not None)


    if kind=='unknown':
        file.write("{ind}# {name}: Could not determine type".format(ind=indent,name=name))
    elif kind=='literal':
        file.write(indent+obj['code'].replace("\n","\n"+indent)+"\n")
    elif kind=='class':
        bases = ", ".join(obj['bases'])
        file.write("{ind}class {name}({bases}):\n".format(
            ind=indent, name=shortname, bases=bases))
        wrote_non_comment = True

        wrote_children = False
        newindent = indent+"    "
        #for c in obj['children-sorted']:

        for mem in obj['children-sorted']:
            if print_object(file=file,name=mem,parent=name,indent=newindent,ismember=True,module=module):
                wrote_children = True

        if not wrote_children:
            file.write("{ind}pass\n".format(ind=newindent))
    elif kind=='module':
        file.write("import typing\n")
        # imports_typed = [] #type: List[str]
        # imports_untyped = [] #type: List[str]
        # for imp in obj['imports']:
        #     if imp in objects:
        #         imports_typed.append(imp)
        #     else:
        #         imports_untyped.append(imp)
        # if imports_typed:
        #     file.write("import {}\n".format(", ".join(imports_typed)))
        # if imports_untyped:
        #     file.write("import {} # type: ignore\n".format(", ".join(imports_untyped)))
        if obj['imports']:
            file.write("import {}\n".format(", ".join(obj['imports'])))
        for c in obj['children-sorted']:
            file.write("\n")
            print_object(file=file,name=c,parent=shortname,module=name)
    elif kind=='function':
        signatures = obj['signatures']
        #file.write("### "+str(signatures)+"\n")
        overload = len(signatures)>1
        for sig in signatures:
            fixed,sig = truncate_fun_sig_deps(name,module,sig,have_printed)
            args = ["{}:{}{}".format(a,typ2str(t),"=None" if d else "")
                    for (a,t,d) in sig['args']]
            ret = typ2str(sig['return'])
            if ismember: args.insert(0,'self')
            if overload: file.write("{ind}@typing.overload\n".format(ind=indent))
            if ismember and shortname != '__init__':
                # TODO this is not not true, but typing-compatible for both
                # class-methods and instance-methods:
                file.write("{ind}@classmethod\n".format(ind=indent))
            file.write("{ind}def {name}({args}) -> {ret}: pass\n".format(
                ind=indent, name=shortname, args=", ".join(args), ret=ret))
            wrote_non_comment = True
            if fixed:
                file.write("{ind}# ERROR: removed types {fix} due to forward dependency\n".format(ind=indent,fix=",".join(fixed)))
        if not signatures:
            file.write("{ind}{name} = None # type:typing.Any\n".format(
                ind=indent, name=shortname)) # TODO Should be a generic callable
            file.write("{ind}# ERROR: No signatures for function/method {name}\n".format(
                ind=indent, name=name))


    elif kind=='value':
        file.write("{ind}{name} = None # type: {typ}\n".format(
            ind=indent, name=shortname, typ=obj['type']))
        wrote_non_comment = True
    elif kind=='copy':
        file.write("{ind}{name} = {original}".format(ind=indent,name=shortname,original=obj['original']))
    else:
        assert False, "Unknown kind "+kind
        
    file.write(errors)
    have_printed.add(name)
    return wrote_non_comment


    

def ismodule(name:str):
    try:
        m=eval(name)
        if inspect.ismodule(m): return True
    except: return False
    return False
        

noimport = {'int','float','str','bytes','None','bool','object'}
def compute_imports():
    for name,m in objects.items():
        if m['kind']!='module': continue
        deps = m['depends-nested']
        deps.update(m['soft-depends-nested'])
        imports = set() #type: Set[str]
        def addT(dd:Type) -> None:
            if isinstance(dd,str):
                if dd in noimport: return
                if len(dd) == 1: return # TODO remove?
                #print("XXX",dd)
                d2 = dd #type:ignore
                module_found = False
                while True:
                    idx = d2.rfind('.')
                    if idx==-1: break
                    d2 = d2[:idx]
                    #print("YYY {} -> {}".format(dd,d2))
                    if ismodule(d2): module_found = True
                    if module_found and d2!=name: imports.add(d2)
                if not module_found:
                    #print(objects[name])
                    raise RuntimeError("Cannot determine module of type {} (when scanning {} for required imports)".format(dd,name))
            else:
                a,b = dd
                addT(a)
                for t in b: addT(t)
        for d in deps:
            addT(d)
        m['imports'] = sorted(imports)



def print_modules():
    filenames = {}

    for m,o in objects.items():
        if o['kind']!='module': continue
        filenames[m] = '.mypy-stubs/{}.py'.format(m.replace('.','/'))

    for m in filenames:
        p = m.split('.')
        for i in range(1,len(p)):
            m2 = ".".join(p[:i])
            filenames[m2] = '.mypy-stubs/{}/__init__.py'.format(m2.replace('.','/'))

    for m,fn in filenames.items():
        os.makedirs(os.path.dirname(fn),exist_ok=True)
        with open(fn,'wt') as f:
            print_object(name=m,file=f)

    # with open(".mypy-stubs/PyQt5/Qt.py","wt") as f:
    #     for m in modules:
    #         f.write("from {} import *\n".format(m.__name__))


def print_stats():
    count = 0
    success = 0
    for obj in objects.values():
        count += 1
        if not obj['errors']: success += 1
    print("Statistics: {} objects, {} with errors, {} error-free".format(
        count,count-success,success))


print("Scanning...")
scan_all()
#print("Test...")
#print(objects['PyQt5.QtCore.QAbstractItemModel.rowCount'])
print("Removing submodule-children...")
remove_module_children()
print("Completing nested dependencies...")
nested_dependencies('depends')
nested_dependencies('soft-depends')
#sys.exit(0);
print("Sorting children...")
sort_all_children()
print("Compute imports")
compute_imports()
print("Output modules...")
print_modules()
print_stats()
