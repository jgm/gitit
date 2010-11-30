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
    configure.cpp \
    newProjectWizard.cpp \
    gitcommand.cpp
HEADERS += mainwindow.h \
    history.h \
    commit.h \
    currentuserinfo.h \
    gitstatusmodel.h \
    configure.h \
    newProjectWizard.h \
    gitcommand.h
FORMS += mainwindow.ui \
    configure.ui
RESOURCES += resources.qrc
