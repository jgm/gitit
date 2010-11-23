/********************************************************************************
** Form generated from reading UI file 'mainwindow.ui'
**
** Created: Mon Nov 22 18:03:27 2010
**      by: Qt User Interface Compiler version 4.6.2
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MAINWINDOW_H
#define UI_MAINWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QListView>
#include <QtGui/QMainWindow>
#include <QtGui/QMenu>
#include <QtGui/QMenuBar>
#include <QtGui/QPushButton>
#include <QtGui/QScrollArea>
#include <QtGui/QSplitter>
#include <QtGui/QStatusBar>
#include <QtGui/QTabWidget>
#include <QtGui/QTextEdit>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_MainWindow
{
public:
    QAction *actionNew;
    QAction *actionExit;
    QAction *actionConfigure;
    QAction *actionAbout;
    QAction *actionOpen;
    QAction *actionUser_s_Manual;
    QWidget *centralWidget;
    QHBoxLayout *horizontalLayout_2;
    QHBoxLayout *horizontalLayout;
    QTabWidget *tabWidget;
    QWidget *localWorkspace;
    QWidget *widget;
    QVBoxLayout *verticalLayout_2;
    QPushButton *pushButton;
    QLabel *label_4;
    QSplitter *splitter_2;
    QLabel *label_5;
    QLineEdit *lineEdit;
    QLabel *label_6;
    QSplitter *splitter_3;
    QLabel *label_7;
    QTextEdit *textEdit;
    QSplitter *splitter_4;
    QListView *changedFileslistView;
    QLabel *label;
    QSplitter *splitter_5;
    QListView *listView_3;
    QLabel *label_2;
    QSplitter *splitter_6;
    QListView *listView_2;
    QLabel *label_3;
    QWidget *localPackageTree;
    QScrollArea *scrollArea;
    QWidget *scrollAreaWidgetContents;
    QSplitter *splitter;
    QPushButton *mergeBranch;
    QPushButton *createNewBranch;
    QPushButton *deleteBranch;
    QPushButton *renameBranch;
    QPushButton *changeCurrentBranch;
    QWidget *remotePackageTree;
    QScrollArea *scrollArea_2;
    QWidget *scrollAreaWidgetContents_2;
    QWidget *widget1;
    QVBoxLayout *verticalLayout;
    QPushButton *pushButton_7;
    QPushButton *pushButton_8;
    QMenuBar *menuBar;
    QMenu *menuFile;
    QMenu *menuHelp;
    QStatusBar *statusBar;

    void setupUi(QMainWindow *MainWindow)
    {
        if (MainWindow->objectName().isEmpty())
            MainWindow->setObjectName(QString::fromUtf8("MainWindow"));
        MainWindow->resize(735, 544);
        actionNew = new QAction(MainWindow);
        actionNew->setObjectName(QString::fromUtf8("actionNew"));
        actionExit = new QAction(MainWindow);
        actionExit->setObjectName(QString::fromUtf8("actionExit"));
        actionConfigure = new QAction(MainWindow);
        actionConfigure->setObjectName(QString::fromUtf8("actionConfigure"));
        actionAbout = new QAction(MainWindow);
        actionAbout->setObjectName(QString::fromUtf8("actionAbout"));
        actionOpen = new QAction(MainWindow);
        actionOpen->setObjectName(QString::fromUtf8("actionOpen"));
        actionUser_s_Manual = new QAction(MainWindow);
        actionUser_s_Manual->setObjectName(QString::fromUtf8("actionUser_s_Manual"));
        centralWidget = new QWidget(MainWindow);
        centralWidget->setObjectName(QString::fromUtf8("centralWidget"));
        horizontalLayout_2 = new QHBoxLayout(centralWidget);
        horizontalLayout_2->setSpacing(6);
        horizontalLayout_2->setContentsMargins(11, 11, 11, 11);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        tabWidget = new QTabWidget(centralWidget);
        tabWidget->setObjectName(QString::fromUtf8("tabWidget"));
        localWorkspace = new QWidget();
        localWorkspace->setObjectName(QString::fromUtf8("localWorkspace"));
        widget = new QWidget(localWorkspace);
        widget->setObjectName(QString::fromUtf8("widget"));
        widget->setGeometry(QRect(450, 100, 201, 201));
        verticalLayout_2 = new QVBoxLayout(widget);
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setContentsMargins(11, 11, 11, 11);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        verticalLayout_2->setContentsMargins(0, 0, 0, 0);
        pushButton = new QPushButton(widget);
        pushButton->setObjectName(QString::fromUtf8("pushButton"));

        verticalLayout_2->addWidget(pushButton);

        label_4 = new QLabel(widget);
        label_4->setObjectName(QString::fromUtf8("label_4"));

        verticalLayout_2->addWidget(label_4);

        splitter_2 = new QSplitter(widget);
        splitter_2->setObjectName(QString::fromUtf8("splitter_2"));
        splitter_2->setOrientation(Qt::Horizontal);
        label_5 = new QLabel(splitter_2);
        label_5->setObjectName(QString::fromUtf8("label_5"));
        splitter_2->addWidget(label_5);
        lineEdit = new QLineEdit(splitter_2);
        lineEdit->setObjectName(QString::fromUtf8("lineEdit"));
        splitter_2->addWidget(lineEdit);

        verticalLayout_2->addWidget(splitter_2);

        label_6 = new QLabel(widget);
        label_6->setObjectName(QString::fromUtf8("label_6"));

        verticalLayout_2->addWidget(label_6);

        splitter_3 = new QSplitter(widget);
        splitter_3->setObjectName(QString::fromUtf8("splitter_3"));
        splitter_3->setOrientation(Qt::Horizontal);
        label_7 = new QLabel(splitter_3);
        label_7->setObjectName(QString::fromUtf8("label_7"));
        splitter_3->addWidget(label_7);
        textEdit = new QTextEdit(splitter_3);
        textEdit->setObjectName(QString::fromUtf8("textEdit"));
        splitter_3->addWidget(textEdit);

        verticalLayout_2->addWidget(splitter_3);

        splitter_4 = new QSplitter(localWorkspace);
        splitter_4->setObjectName(QString::fromUtf8("splitter_4"));
        splitter_4->setGeometry(QRect(40, 120, 151, 141));
        splitter_4->setMinimumSize(QSize(151, 141));
        splitter_4->setOrientation(Qt::Vertical);
        changedFileslistView = new QListView(splitter_4);
        changedFileslistView->setObjectName(QString::fromUtf8("changedFileslistView"));
        splitter_4->addWidget(changedFileslistView);
        label = new QLabel(splitter_4);
        label->setObjectName(QString::fromUtf8("label"));
        splitter_4->addWidget(label);
        splitter_5 = new QSplitter(localWorkspace);
        splitter_5->setObjectName(QString::fromUtf8("splitter_5"));
        splitter_5->setGeometry(QRect(250, 50, 151, 141));
        splitter_5->setOrientation(Qt::Vertical);
        listView_3 = new QListView(splitter_5);
        listView_3->setObjectName(QString::fromUtf8("listView_3"));
        splitter_5->addWidget(listView_3);
        label_2 = new QLabel(splitter_5);
        label_2->setObjectName(QString::fromUtf8("label_2"));
        splitter_5->addWidget(label_2);
        splitter_6 = new QSplitter(localWorkspace);
        splitter_6->setObjectName(QString::fromUtf8("splitter_6"));
        splitter_6->setGeometry(QRect(250, 240, 151, 141));
        splitter_6->setOrientation(Qt::Vertical);
        listView_2 = new QListView(splitter_6);
        listView_2->setObjectName(QString::fromUtf8("listView_2"));
        splitter_6->addWidget(listView_2);
        label_3 = new QLabel(splitter_6);
        label_3->setObjectName(QString::fromUtf8("label_3"));
        splitter_6->addWidget(label_3);
        tabWidget->addTab(localWorkspace, QString());
        localPackageTree = new QWidget();
        localPackageTree->setObjectName(QString::fromUtf8("localPackageTree"));
        scrollArea = new QScrollArea(localPackageTree);
        scrollArea->setObjectName(QString::fromUtf8("scrollArea"));
        scrollArea->setGeometry(QRect(30, 40, 271, 361));
        scrollArea->setWidgetResizable(true);
        scrollAreaWidgetContents = new QWidget();
        scrollAreaWidgetContents->setObjectName(QString::fromUtf8("scrollAreaWidgetContents"));
        scrollAreaWidgetContents->setGeometry(QRect(0, 0, 269, 359));
        scrollArea->setWidget(scrollAreaWidgetContents);
        splitter = new QSplitter(localPackageTree);
        splitter->setObjectName(QString::fromUtf8("splitter"));
        splitter->setGeometry(QRect(440, 120, 169, 159));
        splitter->setOrientation(Qt::Vertical);
        mergeBranch = new QPushButton(splitter);
        mergeBranch->setObjectName(QString::fromUtf8("mergeBranch"));
        splitter->addWidget(mergeBranch);
        createNewBranch = new QPushButton(splitter);
        createNewBranch->setObjectName(QString::fromUtf8("createNewBranch"));
        splitter->addWidget(createNewBranch);
        deleteBranch = new QPushButton(splitter);
        deleteBranch->setObjectName(QString::fromUtf8("deleteBranch"));
        splitter->addWidget(deleteBranch);
        renameBranch = new QPushButton(splitter);
        renameBranch->setObjectName(QString::fromUtf8("renameBranch"));
        splitter->addWidget(renameBranch);
        changeCurrentBranch = new QPushButton(splitter);
        changeCurrentBranch->setObjectName(QString::fromUtf8("changeCurrentBranch"));
        splitter->addWidget(changeCurrentBranch);
        tabWidget->addTab(localPackageTree, QString());
        remotePackageTree = new QWidget();
        remotePackageTree->setObjectName(QString::fromUtf8("remotePackageTree"));
        scrollArea_2 = new QScrollArea(remotePackageTree);
        scrollArea_2->setObjectName(QString::fromUtf8("scrollArea_2"));
        scrollArea_2->setGeometry(QRect(20, 20, 271, 381));
        scrollArea_2->setWidgetResizable(true);
        scrollAreaWidgetContents_2 = new QWidget();
        scrollAreaWidgetContents_2->setObjectName(QString::fromUtf8("scrollAreaWidgetContents_2"));
        scrollAreaWidgetContents_2->setGeometry(QRect(0, 0, 269, 379));
        scrollArea_2->setWidget(scrollAreaWidgetContents_2);
        widget1 = new QWidget(remotePackageTree);
        widget1->setObjectName(QString::fromUtf8("widget1"));
        widget1->setGeometry(QRect(430, 150, 192, 62));
        verticalLayout = new QVBoxLayout(widget1);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(0, 0, 0, 0);
        pushButton_7 = new QPushButton(widget1);
        pushButton_7->setObjectName(QString::fromUtf8("pushButton_7"));

        verticalLayout->addWidget(pushButton_7);

        pushButton_8 = new QPushButton(widget1);
        pushButton_8->setObjectName(QString::fromUtf8("pushButton_8"));

        verticalLayout->addWidget(pushButton_8);

        tabWidget->addTab(remotePackageTree, QString());

        horizontalLayout->addWidget(tabWidget);


        horizontalLayout_2->addLayout(horizontalLayout);

        MainWindow->setCentralWidget(centralWidget);
        menuBar = new QMenuBar(MainWindow);
        menuBar->setObjectName(QString::fromUtf8("menuBar"));
        menuBar->setGeometry(QRect(0, 0, 735, 25));
        menuFile = new QMenu(menuBar);
        menuFile->setObjectName(QString::fromUtf8("menuFile"));
        menuHelp = new QMenu(menuBar);
        menuHelp->setObjectName(QString::fromUtf8("menuHelp"));
        MainWindow->setMenuBar(menuBar);
        statusBar = new QStatusBar(MainWindow);
        statusBar->setObjectName(QString::fromUtf8("statusBar"));
        MainWindow->setStatusBar(statusBar);

        menuBar->addAction(menuFile->menuAction());
        menuBar->addAction(menuHelp->menuAction());
        menuFile->addAction(actionNew);
        menuFile->addAction(actionOpen);
        menuFile->addAction(actionConfigure);
        menuFile->addAction(actionExit);
        menuHelp->addAction(actionAbout);
        menuHelp->addAction(actionUser_s_Manual);

        retranslateUi(MainWindow);

        tabWidget->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(MainWindow);
    } // setupUi

    void retranslateUi(QMainWindow *MainWindow)
    {
        MainWindow->setWindowTitle(QApplication::translate("MainWindow", "MainWindow", 0, QApplication::UnicodeUTF8));
        actionNew->setText(QApplication::translate("MainWindow", "New", 0, QApplication::UnicodeUTF8));
        actionExit->setText(QApplication::translate("MainWindow", "Exit", 0, QApplication::UnicodeUTF8));
        actionConfigure->setText(QApplication::translate("MainWindow", "Configure", 0, QApplication::UnicodeUTF8));
        actionAbout->setText(QApplication::translate("MainWindow", "About", 0, QApplication::UnicodeUTF8));
        actionOpen->setText(QApplication::translate("MainWindow", "Open", 0, QApplication::UnicodeUTF8));
        actionUser_s_Manual->setText(QApplication::translate("MainWindow", "User's Manual", 0, QApplication::UnicodeUTF8));
        pushButton->setText(QApplication::translate("MainWindow", "Ship", 0, QApplication::UnicodeUTF8));
        label_4->setText(QApplication::translate("MainWindow", "with", 0, QApplication::UnicodeUTF8));
        label_5->setText(QApplication::translate("MainWindow", "name", 0, QApplication::UnicodeUTF8));
        label_6->setText(QApplication::translate("MainWindow", "and", 0, QApplication::UnicodeUTF8));
        label_7->setText(QApplication::translate("MainWindow", "description", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("MainWindow", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'Sans'; font-size:10pt; font-weight:400; font-style:normal;\">\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Changed Files</p></body></html>", 0, QApplication::UnicodeUTF8));
        label_2->setText(QApplication::translate("MainWindow", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'Sans'; font-size:10pt; font-weight:400; font-style:normal;\">\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Unchanged Files</p></body></html>", 0, QApplication::UnicodeUTF8));
        label_3->setText(QApplication::translate("MainWindow", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'Sans'; font-size:10pt; font-weight:400; font-style:normal;\">\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Ignored Files</p></body></html>", 0, QApplication::UnicodeUTF8));
        tabWidget->setTabText(tabWidget->indexOf(localWorkspace), QApplication::translate("MainWindow", "Local Workspace", 0, QApplication::UnicodeUTF8));
        mergeBranch->setText(QApplication::translate("MainWindow", "Merge Branches", 0, QApplication::UnicodeUTF8));
        createNewBranch->setText(QApplication::translate("MainWindow", "Create New Branch", 0, QApplication::UnicodeUTF8));
        deleteBranch->setText(QApplication::translate("MainWindow", "Delete Branch", 0, QApplication::UnicodeUTF8));
        renameBranch->setText(QApplication::translate("MainWindow", "Rename Branch", 0, QApplication::UnicodeUTF8));
        changeCurrentBranch->setText(QApplication::translate("MainWindow", "Change Current Branch", 0, QApplication::UnicodeUTF8));
        tabWidget->setTabText(tabWidget->indexOf(localPackageTree), QApplication::translate("MainWindow", "Local Package Tree", 0, QApplication::UnicodeUTF8));
        pushButton_7->setText(QApplication::translate("MainWindow", "Sync to Remote Branch", 0, QApplication::UnicodeUTF8));
        pushButton_8->setText(QApplication::translate("MainWindow", "Sync From Remote Branch", 0, QApplication::UnicodeUTF8));
        tabWidget->setTabText(tabWidget->indexOf(remotePackageTree), QApplication::translate("MainWindow", "Remote Package Tree", 0, QApplication::UnicodeUTF8));
        menuFile->setTitle(QApplication::translate("MainWindow", "File", 0, QApplication::UnicodeUTF8));
        menuHelp->setTitle(QApplication::translate("MainWindow", "Help", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class MainWindow: public Ui_MainWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MAINWINDOW_H
