import contextlib, logging,  os, sys
from lxml import etree # type: ignore # TODO dont ignore @UnresolvedImport

class WithinBlock(contextlib.ContextDecorator): # type: ignore
    def __init__(self, catchall=True, info=None):
        self.active = False
        self.info = info
        self.catchall = catchall
        
    def __enter__(self):
        if self.active: raise RuntimeError("WithinBlock nested")
        self.active = True
        
    def __exit__(self,  exc, excval, trace):
        self.active = False
        if exc is not None and self.catchall:
            logging.warning("Unhandled exception: {} {} {}".format(exc, excval, trace))
            return True

# Directory which contains the distribution
if getattr(sys, 'frozen', False):
    # The application is frozen
    base_path = os.path.dirname(sys.executable)
else:
    # The application is not frozen
    base_path = os.path.abspath(os.path.join(os.path.dirname(__file__),'..'))
logging.info("Base path: %s",base_path)

def file_path(file):
    """Interprets file as a path relative to the project/installation directory, 
    and returns an absolute path."""
    return os.path.join(base_path,file)

def dump_xml(xml, depth=2):
    """Converts XML to a string, but with limited depth only."""
    def copy(xml, depth):
        new = xml.makeelement(xml.tag, xml.attrib, xml.nsmap)
        new.tail = xml.tail
        if depth>0:
            new.text = xml.text
            for e in xml:
                new.append(copy(e, depth-1))
        elif xml.text is not None or len(xml)>0:
            new.text = '...'
        return new
    
    return etree.tostring(copy(xml, depth))
