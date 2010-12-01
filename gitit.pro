# -------------------------------------------------
# Project created by QtCreator 2010-10-28T20:47:00
# -------------------------------------------------
QT += core \
    gui
TARGET = gitit
TEMPLATE = app
ICON = images/gitit.icns
SOURCES += main.cpp \
    mainwindow.cpp \
    history.cpp \
    commit.cpp \
    currentuserinfo.cpp \
    gitchangedstatusmodel.cpp \
    configure.cpp \
    newProjectWizard.cpp \
    gitcommand.cpp \
    gitstagedstatusmodel.cpp
HEADERS += mainwindow.h \
    history.h \
    commit.h \
    currentuserinfo.h \
    gitchangedstatusmodel.h \
    configure.h \
    newProjectWizard.h \
    gitcommand.h \
    gitstagedstatusmodel.h
FORMS += mainwindow.ui \
    configure.ui
RESOURCES += resources.qrc
