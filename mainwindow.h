#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
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
    GitStatusModel* gitStatusModel;

};

#endif // MAINWINDOW_H
