#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

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

signals:
    void repositoryChanged(QString repo);

private slots:
    void on_actionOpen_triggered();
    void on_actionConfigure_triggered();
    void boxClicked();
};

#endif // MAINWINDOW_H
