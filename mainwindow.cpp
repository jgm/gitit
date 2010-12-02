#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "gitchangedstatusmodel.h"
#include "configure.h"
#include <QFileDialog>
#include <QDebug>
#include <QMessageBox>
#include "gitcommand.h"
#include "gitstagedstatusmodel.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    configure(new Configure(this)),
    gitChangedStatusModel(new GitChangedStatusModel),
    gitStagedStatusModel(new GitStagedStatusModel),
    gitIgnoredFilesModel(new QStringListModel),
    newProjectWizard( new NewProjectWizard),
    gitCommand(new GitCommand)
{
    ui->setupUi(this);
    ui->changedFileslistView->setModel(gitChangedStatusModel);
    ui->stagedFilesListView->setModel(gitStagedStatusModel);
    ui->ignoredFilesListView->setModel(gitIgnoredFilesModel);
    //connecting slots and signals
    connect(gitCommand,SIGNAL(status(QStringList)),gitChangedStatusModel,SLOT(update(QStringList)));
    connect(gitCommand,SIGNAL(status(QStringList)),gitStagedStatusModel,SLOT(update(QStringList)));
    connect( ui->actionAbout, SIGNAL( triggered() ), this, SLOT(about()) );
    connect( ui->actionExit, SIGNAL( triggered() ), this, SLOT(exit()) );
    connect( ui->actionNew, SIGNAL( triggered() ), this, SLOT(menuNew()) );
    connect( ui->actionUser_s_Manual, SIGNAL( triggered() ), this, SLOT(userManual()) );
    connect( ui->actionNew_2, SIGNAL(triggered()), this, SLOT(activateNewProjectWizard()) );
    connect( ui->actionRemote_Repository, SIGNAL(triggered()), this, SLOT(activateShareProjectWizard()) );
    //using a builtin model here:
    connect(gitCommand, SIGNAL(lsIgnored(QStringList)), this, SLOT(updateIgnoredModel(QStringList)));

}

MainWindow::~MainWindow()
{
    delete ui;
    delete gitChangedStatusModel;
    delete configure;
    delete gitStagedStatusModel;
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
    this->gitCommand->setRepo(repo);
}

void MainWindow::boxClicked()
{
    ui->statusBar->showMessage("box", 15000);
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
    this->close();
}

void MainWindow::menuNew()
{

}

void MainWindow::userManual()
{

}

void MainWindow::activateNewProjectWizard()
{
    newProjectWizard->show();
}

void MainWindow::activateShareProjectWizard()
{

}
void MainWindow::updateIgnoredModel(QStringList files)
{
    gitIgnoredFilesModel->setStringList(files);
}

void MainWindow::on_gitAddButton_clicked()
{
    //ui->git
    //QModelIndexList *indexList = ui->changedFileslistView->selectedIndexes();
    /* (int i=0; i < indexList->count(); ++i)
    {
        indexList->at(i);
    } */
}
