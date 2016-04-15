import logging_conf; logging_conf.dummy # To get rid of the IDE warning

from PyQt5.QtWidgets import QApplication
from testui.mainwin import MainWin
from PyQt5 import QtWebKit
from PyQt5.QtGui import QIcon
import sys, utils, traceback

# TODO: give windows an extra zoom factor of 2.26

# TODO: when switching to single input trafo, keep currently active input

#AB<div id="measure" style="visibility:hidden;position:fixed;">X</div>CD
#
#<script>
#
#div = document.getElementById("measure");
#div.style.width = "1000ex";
#exwidth = div.offsetWidth;
#div.style.width = "1000cm";
#cmwidth = div.offsetWidth;
#exsize = exwidth / cmwidth;
#console.log(exsize);
#
#</script>


def main():
    app = QApplication(sys.argv)
    QtWebKit.QWebSettings.globalSettings().setAttribute(QtWebKit.QWebSettings.DeveloperExtrasEnabled, True) # type: ignore
    QIcon.setThemeName("icons") # type: ignore
    QIcon.setThemeSearchPaths([utils.file_path("resources")]) # type: ignore
    ui = MainWin()
    ui.showMaximized() # type: ignore
    return app.exec_()

if __name__ == "__main__":
    if sys.platform == 'win32':
        rc = 255
        try: rc = main()
        except Exception as e:
            traceback.print_exc()
        input("Press Enter to close")
        sys.exit(rc)
    else:
        sys.exit(main())
        
