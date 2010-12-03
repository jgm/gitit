#ifndef CONFIGURE_H
#define CONFIGURE_H

#include <QDialog>
#include <QSettings>
class GitCommand;
namespace Ui {
    class Configure;
}

class Configure : public QDialog
{
    Q_OBJECT

public:
    explicit Configure(QWidget *parent = 0, GitCommand* gitCommand = 0);
    ~Configure();
public slots:
    void accept();
    void reject();
private:
    Ui::Configure *ui;
    QSettings settings;
    GitCommand* gitCommand;
};

#endif // CONFIGURE_H
