#!/usr/bin/python3

import html

# TODO make immutable
# TODO support cdbase-attrib (inheritable?)

popcorn_shortcuts = {
    "transc1.cos": "cos",
    "transc1.cosh": "cosh",
    "transc1.cot": "cot",
    "transc1.coth": "coth",
    "transc1.csc": "csc",
    "transc1.csch": "csch",
    "transc1.exp": "exp",
    "transc1.sec": "sec",
    "transc1.sech": "sech",
    "transc1.sin": "sin",
    "transc1.sinh": "sinh",
    "transc1.tan": "tan",
    "transc1.tanh": "tanh",
    "arith1.abs": "abs",
    "arith1.root": "root",
    "arith1.sum": "sum",
    "arith1.product": "product",
    "calculus1.diff": "diff",
    "calculus1.int": "int",
    "calculus1.defint": "defint",
    "nums1.pi": "pi",
    "nums1.e": "e",
    "nums1.i": "i",
    "nums1.infinity": "infinity",
    "minmax1.min": "min",
    "minmax1.max": "max",
    "fns1.lambda": "lambda",
    "logic1.true": "true",
    "logic1.false": "false",
    "combinat1.binomial": "binomial",
    "integer1.factorial": "factorial",
}

popcorn_infix = { # True = arity n, False = arity 2
    "prog1.block": (";",True,1),
    "prog1.assign": (":=",False,2),
    "logic1.implies": ("==>",False,3),
    "logic1.equivalent": ("<=>",False,4),
    "logic1.or": ("or",True,5),
    "logic1.and": ("and",True,6),
    "relation1.lt": ("<",False,7),
    "relation1.leq": ("<=",False,8),
    "relation1.gt": (">",False,9),
    "relation1.eq": ("=",False,10),
    "relation1.geq": (">=",False,11),
    "relation1.neq": ("<>",False,12),
    "relation1.neq": ("!=",False,13),
    "interval1.interval": ("..",True,14),
    "arith1.plus": ("+",True,15),
    "arith1.minus": ("-",False,16),
    "arith1.times": ("*",True,17),
    "arith1.divide": ("/",False,18),
    "arith1.power": ("^",False,19),
    "complex1.complex_cartesian": ("|",False,20),
    "nums1.rational": ("//",False,21),
    "list1.list": ((lambda a: "["+",".join(a)+']'),True,22),
    "set1.set": ((lambda a: "{"+",".join(a)+'}'),True,23),
# TODO: if...then...else...endif	prog1.if	n	Not really a infix operator
# TODO: while...do...endwhile	prog1.while	n	Not really a infix operator
}

def infix_renderer(sym):
    def r(headname, head, args):
        assert len(args)==2
        return '<span class="application-infix"><span class="infix-arg">{}</span><span class="infix-operator">{}</span><span class="infix-arg">{}</span></span>'.format(args[0], sym, args[1])
    return r
def prefix_renderer(sym):
    def r(headname, head, args):
        assert len(args)==1
        return '<span class="application-prefix"><span class="prefix-operator">{}</span>{}</span>'.format(sym, args[0])
    return r
def concat_renderer(headname, head, args):
    return '<span class="application-concat">{}</span>'.format("".join(args))
def superscript_renderer(headname, head, args):
    assert len(args)==2
    return '<span class="application-superscript">{}<sup>{}</sup></span>'.format(args[0], args[1])
def fraction_renderer(headname, head, args):
    assert len(args)==2
    return '<span class="fraction"><span class="fraction-nomin">{}</span><span class="fraction-denom">{}</span></span>'.format(args[0], args[1])
def sqrt_renderer(headname, head,  args):
    assert len(args)==1
    return '<span class="sqrt">&radic;<span class="sqrt-arg">{}</span></span>'.format(args[0])

html_renderers = {
    "relation1.eq": infix_renderer("="), 
    "arith1.unary_minus": prefix_renderer("&minus;"), 
    "arith1.minus": infix_renderer("&minus;"), 
    "arith1.times": concat_renderer, 
    "arith1.power": superscript_renderer, 
    "multiops.plusminus": infix_renderer("&pm;"), 
    "arith1.divide": fraction_renderer, 
    "arith1.root": sqrt_renderer, 
}

def infix_mml_renderer(sym):
    sym_mml = '<mo class="leaf" form="infix">{}</mo>'.format(sym)
    def r(headname, head, attrs, args):
        assert len(args) >= 2
        return "<mrow {}>{}</mrow>".format(attrs, sym_mml.join(args))
    return r
def prefix_mml_renderer(sym):
    sym_mml = '<mo class="leaf" form="prefix">{}</mo>'.format(sym)
    def r(headname, head, attrs, args):
        assert len(args)==1
        return "<mrow {}>{}{}</mrow>".format(attrs, sym_mml, args[0])
    return r
def superscript_mml_renderer(headname, head, attrs, args):
    assert len(args)==2
    return '<msup {}>{}{}</msup>'.format(attrs, args[0], args[1])
def fraction_mml_renderer(headname, head, attrs, args):
    assert len(args)==2
    return '<mfrac {}>{}{}</mfrac>'.format(attrs, args[0], args[1])
def sqrt_mml_renderer(headname, head, attrs, args):
    assert len(args)==1
    # The vertical space is used so that it becomes possible to select the sqrt by mouse
    return '<msqrt {}><mspace height="1ex" class="leaf"/>{}</msqrt>'.format(attrs, args[0])

mathml_renderers = {
    "relation1.eq": infix_mml_renderer("="), 
    "arith1.minus": infix_mml_renderer("&minus;"), 
    "multiops.plusminus": infix_mml_renderer("&pm;"), 
    "arith1.times":  infix_mml_renderer("&InvisibleTimes;"), 
    "arith1.unary_minus": prefix_mml_renderer("&minus;"), 
    "arith1.power": superscript_mml_renderer, 
    "arith1.divide": fraction_mml_renderer, 
    "arith1.root": sqrt_mml_renderer, 
}

def from_xml(xml):
    """Converts an OpenMath xml fragment to a OpenMath object.
    
    xml - the XML code as an lxml.Element"""

    om = None
    if (xml.tag == '{http://www.openmath.org/OpenMath}OMOBJ'):
        om = from_xml(xml[0])
    elif (xml.tag == '{http://www.openmath.org/OpenMath}OMA'):
        om = Appl(*(from_xml(child) for child in xml))
    elif (xml.tag == '{http://www.openmath.org/OpenMath}OMS'):
        om = Sym(xml.get('cd'),xml.get('name'))
    elif (xml.tag == '{http://www.openmath.org/OpenMath}OMV'):
        om = Var(xml.get('name'))
    elif (xml.tag == '{http://www.openmath.org/OpenMath}OMI'):
        om = Int(int(xml.text))
    elif (xml.tag == '{http://www.openmath.org/OpenMath}OMBIND'):
        binder = from_xml(xml[0])
        bvars = tuple(from_xml(v) for v in xml[1])
        args = from_xml(xml[2])
        om = Bind(binder,bvars,args)
    elif (xml.tag == '{http://www.openmath.org/OpenMath}OMATTR'):
        om = from_xml(xml[1])
        attr_iter = xml[0].__iter__()
        for key in attr_iter:
            key = from_xml(key)
            assert isinstance(key,Sym)
            val = from_xml(attr_iter.next())
            om.attributes.append((key,val))
    else:
        raise ValueError("Unexpected tag {} in OpenMath XML".format(xml.tag))

    if xml.get('id') is not None: om.id = xml.get('id')
    return om

class OpenMath:
    """Represents an openmath formula (OMS,OMV,OMI,OMB,OMSTR,OMF,OMA,OMBIND,OME,OMATTR,OMR)"""
    def __init__(self):
        self.id = None
        self.parent = None
        self.attributes = []
    def to_popcorn(self,pri=0):
        pop = self._to_popcorn(pri)
        if self.attributes:
            pop += "{"+", ".join(k.to_popcorn(0)+"->"+v.to_popcorn(0)
                                 for (k,v) in self.attributes.items())+"}"
        if self.id is not None:
            pop = pop+":"+self.id
        return pop
    def _to_popcorn(self,pri):
        raise NotImplementedError("{}.to_popcorn()".format(type(self)))
    def __str__(self):
        return "<OpenMath: {}>".format(self.to_popcorn(0))
    def find(self,path,drop=0):
        if len(path)<=drop: return self
        return self._find(path,drop)
    def replace_copy(self,a,b):
        if a==self: return b
        copy = self._replace_copy(a,b)
        copy.id = self.id
        copy.attributes = [(k.replace_copy(a,b),v.replace_copy(a,b))
                           for (k,v) in self.attributes]
        return copy
    def _replace_copy(self,a,b):
        raise NotImplementedError("{}._replace_copy".format(type(self)))
    def __eq__(self,a):
        if type(self)!=type(a): return False
        if self.attributes != a.attributes: return False
        return self._eq(a)
    def _eq(self,a):
        raise NotImplementedError("{}.__eq__".format(type(self)))
    def to_html(self):
        raise NotImplementedError("{}.to_html".format(type(self)))
    def to_mathml(self, path):
        raise NotImplementedError("{}.to_mathml".format(type(self)))
    def path_to_str(self, path):
        return ".".join(str(x) for x in path)


class Int(OpenMath): # OMI
    def __init__(self,value):
        assert isinstance(value,int)
        super().__init__()
        self.value = value
    def _to_popcorn(self,pri):
        return str(self.value)
    def _find(self,path,drop):
        raise ValueError("pointer descends into Int")
    def _eq(self,a):
        return self.value==a.value
    def _replace_copy(self,a,b):
        return Int(self.value)
    def to_html(self):
        return '<span class="integer">{}</span>'.format(str(self.value))
    def to_mathml(self, path):
        return '<mn class="leaf" path="{}">{}</mn>'.format(self.path_to_str(path), str(self.value))

class Sym(OpenMath): # OMS
    def __init__(self,cd,name):
        assert isinstance(cd,str)
        assert isinstance(name,str)
        super().__init__()
        self.cd = cd
        self.name = name
    def _to_popcorn(self,pri):
        rep = self.cd+"."+self.name
        short = popcorn_shortcuts.get(rep)
        if short is not None: return short
        return rep
    def _find(self,path,drop):
        raise ValueError("pointer descends into Sym")
    def _eq(self,a):
        return self.cd==a.cd and self.name==a.name
    def _replace_copy(self,a,b):
        return Sym(self.cd,self.name)
    def to_html(self):
        return '<span class="symbolname">{}</span>'.format(html.escape(self.cd+"."+self.name))
    def to_mathml(self, path):
        return '<mi class="symbolname leaf" path="{}">{}</mi>'.format(self.path_to_str(path), html.escape(self.cd+"."+self.name))

class Var(OpenMath): # OMV
    def __init__(self,name):
        super().__init__()
        self.name = name
    def _to_popcorn(self,pri):
        return "$"+self.name
    def _find(self,path,drop):
        raise ValueError("pointer descends into Var")
    def to_html(self):
        return '<span class="variable">{}</span>'.format(html.escape(self.name))
    def to_mathml(self, path):
        return '<mi class="variable leaf" path="{}">{}</mi>'.format(self.path_to_str(path),   html.escape(self.name))

class Appl(OpenMath): # OMA
    def __init__(self,*args):
        for a in args: assert isinstance(a,OpenMath)
        assert(args) # Must not be empty
        super().__init__()
        for a in args: a.parent = self
        self.args = args
    def _to_popcorn(self,pri):
        head = self.args[0].to_popcorn(100)
        args = [a.to_popcorn(0) for a in self.args[1:]]
        infix = popcorn_infix.get(head)
        if infix is not None:
            (sym,multi,pri2) = infix
            if not multi:
                if len(args)==2:
                    res = "{} {} {}".format(args[0],sym,args[1])
                    if pri2<=pri: res="("+res+")"
                    return res
            else:
                if isinstance(sym,str) and len(args)>=2:
                    res = (" "+sym+" ").join(args)
                    if pri2<=pri: res="("+res+")"
                    return res
                else:
                    return sym(args)
        return "{}({})".format(head,", ".join(args))
    def _find(self,path,drop):
        return self.args[path[drop]].find(path,drop+1)
    def _eq(self,a):
        return self.args==a.args
    def _replace_copy(self,a,b):
        return Appl(*tuple(x.replace_copy(a,b) for x in self.args))
    def to_html(self):
        head = self.args[0]
        if isinstance(head, Sym):
            headname = head.cd+"."+head.name
            renderer = html_renderers.get(headname)
            if renderer:
                return renderer(headname,head,[a.to_html() for a in self.args[1:]])
        return """<span class="application"><span class="application-head">{}</span>({})</span>""".format(
                            head.to_html(),  ",".join('<span class="application-arg">{}</span>'.format(a.to_html()) for a in self.args[1:]))
    def to_mathml(self, path):
        attrs = 'path="{}"'.format(self.path_to_str(path))
        head = self.args[0]
        import itertools
        def to_mathml(a, i): path.append(i); res=a.to_mathml(path); path.pop(); return res
        args = [to_mathml(a, i) for (i, a) in zip(itertools.count(1), self.args[1:])]
        if isinstance(head, Sym):
            headname = head.cd+"."+head.name
            renderer = mathml_renderers.get(headname)
            if renderer:
                return renderer(headname,head,attrs, args)
        return """<mrow {}>{}<mo>&ApplyFunction;</mo><mfenced>{}</mfenced></mrow>""".format(
                            attrs, to_mathml(head, 0),  "".join(args))
        


class Bind(OpenMath):
    def __init__(self,binder,bvars,arg):
        super().__init__()
        assert isinstance(binder,OpenMath)
        assert isinstance(bvars,tuple)
        for v in bvars: assert isinstance(v,Var)
        assert isinstance(arg,OpenMath)
        self.binder = binder
        self.bvars = bvars
        self.arg = arg
        binder.parent = self
        arg.parent = self
        for a in bvars: a.parent = self
    def _to_popcorn(self):
        return "{}[{} -> {}]".format(self.binder.to_popcorn(),
                                     ",".join(v.to_popcorn() for v in self.bvars),
                                     self.arg.to_popcorn())
    def _find(self,path,drop):
        i = path[drop]
        if i==0:
            return self.binder.find(path,drop+1)
        elif i==1:
            assert len(path)==drop+2
            return self.bvars[path[drop+1]]
        elif i==2:
            return self.arg.find(path,drop+1)
        else:
            raise ValueError("invalid index in find for Bind")




