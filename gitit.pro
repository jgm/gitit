#-------------------------------------------------
#
# Project created by QtCreator 2010-10-28T20:47:00
#
#-------------------------------------------------

QT       += core gui

TARGET = gitit
TEMPLATE = app


SOURCES += main.cpp\
    mainwindow.cpp \
    history.cpp \
    commit.cpp \
    currentuserinfo.cpp \
    gitstatusmodel.cpp \
    configure.cpp

HEADERS  += mainwindow.h \
    history.h \
    commit.h \
    currentuserinfo.h \
    gitstatusmodel.h \
    configure.h

FORMS    += mainwindow.ui \
            configure.ui
INCLUDEPATH += libgit2/src
LIBS += -L$$PWD/libgit2/ -lgit2 -lz

#unix:LIBS += -Llibgit2/ -lgit2
 #win32:LIBS += c:/mylibs/math.lib
