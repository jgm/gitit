# -------------------------------------------------
# Project created by QtCreator 2010-10-28T20:47:00
# -------------------------------------------------
QT += core \
    gui
TARGET = gitit
TEMPLATE = app
ICON = gitit.icns
SOURCES += main.cpp \
    mainwindow.cpp \
    history.cpp \
    commit.cpp \
    currentuserinfo.cpp \
    gitstatusmodel.cpp \
    configure.cpp
HEADERS += mainwindow.h \
    history.h \
    commit.h \
    currentuserinfo.h \
    gitstatusmodel.h \
    configure.h
FORMS += mainwindow.ui \
    configure.ui
INCLUDEPATH += libgit2/src

# Build libgit2
unix:libgit2.target = ../gitit/libgit2/libgit2.a
unix:libgit2.commands = "make -C ../gitit/libgit2"
unix:libgit2.depends = FORCE
unix:PRE_TARGETDEPS += ../gitit/libgit2/libgit2.a
unix:QMAKE_EXTRA_TARGETS += libgit2
unix:LIBS += -L$$PWD/libgit2/ \
    -lgit2
unix:LIBS += -lz # OS X requires a dynamic link to zlib
win32:LIBS += -L$$PWD/external-win32/ \
    -lgit2
OTHER_FILES += 
RESOURCES += resources.qrc
