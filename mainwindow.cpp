#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "gitstatusmodel.h"
#include "configure.h"
#include <QFileDialog>
#include <QDebug>
MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    configure(new Configure(this)),
    gitStatusModel(new GitStatusModel)
{
    ui->setupUi(this);
    ui->changedFileslistView->setModel(gitStatusModel);
    //connecting slots and signals
    connect(this,SIGNAL(repositoryChanged(git_repository*)),gitStatusModel,SLOT(update(git_repository*)));
    connect(ui->box, SIGNAL(linkActivated(QString)), this, SLOT(boxClicked()) );
    connect( ui->actionAbout, SIGNAL( triggered() ), this, SLOT(about()) );
    connect( ui->actionExit, SIGNAL( triggered() ), this, SLOT(exit()) );
    connect( ui->actionNew, SIGNAL( triggered() ), this, SLOT(menuNew()) );
    connect( ui->actionUser_s_Manual, SIGNAL( triggered() ), this, SLOT(userManual()) );
}

MainWindow::~MainWindow()
{
    delete ui;
    delete gitStatusModel;
    delete configure;

}

void MainWindow::on_actionConfigure_triggered()
{
    configure->show();
}

void MainWindow::on_actionOpen_triggered()
{
    QStringList fileNames;

    QFileDialog dialog(this);
    dialog.setFileMode(QFileDialog::Directory);
    dialog.setViewMode(QFileDialog::Detail);
    dialog.setOption(QFileDialog::ShowDirsOnly);

    if (dialog.exec())
        fileNames = dialog.selectedFiles();

    repo = fileNames[0];
}

void MainWindow::boxClicked()
{
    ui->statusBar->showMessage("box", 15);
}

void MainWindow::about()
{
    QMessageBox::about(this, tr("About Gitit"),
                       tr("Gitit, Created for CS440 Fall 2010<br>"
                          "For access to source code, goto http://github.com/bdenne2/gitit<br>"
                          "Bryanna Denney &lt;bryanna.denney@gmail.com&gt;<br>"
                          "Mike Salata &lt;mbaker22@uic.edu&gt;<br>"
                          "Steven Sennebogen &lt;ssennebo@cs.uic.edu&gt;"));
}

void MainWindow::exit()
{

}

void MainWindow::menuNew()
{

}

void MainWindow::userManual()
{

}
