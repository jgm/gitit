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
    gitchangedstatusmodel.cpp \
    configure.cpp \
    gitcommand.cpp \
    gitstagedstatusmodel.cpp \
    existingProjectWizard.cpp \
    newProjectWizard.cpp \
    remoteWizard.cpp
HEADERS += mainwindow.h \
    gitchangedstatusmodel.h \
    configure.h \
    gitcommand.h \
    gitstagedstatusmodel.h \
    existingProjectWizard.h \
    newProjectWizard.h \
    remoteWizard.h
FORMS += mainwindow.ui \
    configure.ui
RESOURCES += resources.qrc
