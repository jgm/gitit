#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "gitchangedstatusmodel.h"
#include "configure.h"
#include <QFileDialog>
#include <QDebug>
#include <QMessageBox>
#include "gitcommand.h"
#include "gitstagedstatusmodel.h"
#include <QStringList>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    configure(new Configure(this)),
    gitChangedStatusModel(new GitChangedStatusModel),
    gitStagedStatusModel(new GitStagedStatusModel),
    gitIgnoredFilesModel(new QStringListModel),
    existingProjectWizard( new ExistingProjectWizard),
    newProjectWizard( new NewProjectWizard),
    gitCommand(new GitCommand)

{
    ui->setupUi(this);
    ui->changedFileslistView->setModel(gitChangedStatusModel);
    ui->stagedFilesListView->setModel(gitStagedStatusModel);
    ui->ignoredFilesListView->setModel(gitIgnoredFilesModel);
    //connecting slots and signals
    connect( gitCommand, SIGNAL(status(QStringList)), gitChangedStatusModel, SLOT(update(QStringList)));
    connect( gitCommand, SIGNAL(status(QStringList)), gitStagedStatusModel,  SLOT(update(QStringList)));

    connect( ui->actionAbout, SIGNAL( triggered() ), this, SLOT(about()) );
    connect( ui->actionExit, SIGNAL( triggered() ), this, SLOT(exit()) );
    connect( ui->actionNew, SIGNAL( triggered() ), this, SLOT(menuNew()) );
    connect( ui->actionUser_s_Manual, SIGNAL( triggered() ), this, SLOT(userManual()) );
    connect( ui->actionExisting_Project, SIGNAL(triggered()), this, SLOT(activateExistingProjectWizard()) );
    connect( ui->actionNew, SIGNAL(triggered()), this, SLOT(activateNewProjectWizard()) );
    connect( ui->actionNew_Project, SIGNAL(triggered()), this, SLOT(activateNewProjectWizard()));
    //using a builtin model here:
    connect( gitCommand, SIGNAL(lsIgnored(QStringList)), this, SLOT(updateIgnoredModel(QStringList)));
    connect( gitCommand, SIGNAL(log(QString))          , this, SLOT(updateLog(QString) ));

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
    reload();
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

void MainWindow::activateExistingProjectWizard()
{
    existingProjectWizard->restart();
    existingProjectWizard->clear();
    existingProjectWizard->show();
}

void MainWindow::activateNewProjectWizard()
{
    newProjectWizard->restart();
    newProjectWizard->clear();
    newProjectWizard->show();
}

void MainWindow::updateIgnoredModel(QStringList files)
{
    gitIgnoredFilesModel->setStringList(files);
}

void MainWindow::updateLog(QString log)
{
    ui->gitLogTextEdit->setPlainText(log);
}

void MainWindow::on_gitAddButton_clicked()
{
    QItemSelectionModel* selectionModel = ui->changedFileslistView->selectionModel();
    QItemSelection itemSelection = selectionModel->selection();
    QModelIndexList indexList = itemSelection.indexes();
    for(int i=0; i < indexList.count(); ++i)
    {
        QString filename = QString(indexList.at(i).data().toString());
        gitCommand->add( filename );

        ui->statusBar->showMessage(filename,5000);
    }
    reload();
}

void MainWindow::reload()
{
    gitCommand->status();
    gitCommand->lsIgnored();
    gitCommand->log();
}

void MainWindow::on_shipButton_clicked()
{
    QStringList args;
    QString description;
    description = ui->commitName->text();
    description += "\n\n" + ui->commitDescription->toPlainText();
    args << "commit" << "-m" << description;
    gitCommand->run(args);
    reload();
}

void MainWindow::on_reload_clicked()
{
    reload();
}

void MainWindow::on_syncToButton_clicked()
{
    //TODO: fixme
}
