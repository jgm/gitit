#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "git/repository.h"
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
    git_repository* repo;

signals:
    void repositoryChanged(git_repository* repo);

private slots:
    void on_graphicsView_customContextMenuRequested(QPoint pos);
    void on_actionOpen_triggered();
    void on_actionConfigure_triggered();
};

#endif // MAINWINDOW_H
