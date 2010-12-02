#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QStringListModel>
#include "newProjectWizard.h"
//#include "shareProjectWizard.h"

class Configure;
class GitChangedStatusModel;
class GitCommand;
class GitStagedStatusModel;

namespace Ui {
    class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

private:
    Ui::MainWindow *ui;
    Configure* configure;
    GitChangedStatusModel* gitChangedStatusModel;
    GitStagedStatusModel* gitStagedStatusModel;
    QStringListModel* gitIgnoredFilesModel;
    QString repo;
    NewProjectWizard* newProjectWizard;
//    ShareProjectWizard shareProjectWizard;
    GitCommand* gitCommand;

signals:
    void repositoryChanged(QString repo);

private slots:
    void on_gitAddButton_clicked();
    void on_actionOpen_triggered();
    void on_actionConfigure_triggered();
    void boxClicked();
    void about();
    void exit();
    void menuNew();
    void userManual();
    void activateNewProjectWizard();
    void activateShareProjectWizard();
    void updateIgnoredModel(QStringList);
};

#endif // MAINWINDOW_H
