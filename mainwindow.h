#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "newProjectWizard.h"
//#include "shareProjectWizard.h"

class Configure;
class GitStatusModel;

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
    GitStatusModel* gitStatusModel;
    QString repo;
    NewProjectWizard* newProjectWizard;
//    ShareProjectWizard shareProjectWizard;

signals:
    void repositoryChanged(QString repo);

private slots:
    void on_actionOpen_triggered();
    void on_actionConfigure_triggered();
    void boxClicked();
    void about();
    void exit();
    void menuNew();
    void userManual();
    void activateNewProjectWizard();
    void activateShareProjectWizard();
};

#endif // MAINWINDOW_H
