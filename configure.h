#ifndef CONFIGURE_H
#define CONFIGURE_H

#include <QDialog>
#include <QSettings>

namespace Ui {
    class Configure;
}

class Configure : public QDialog
{
    Q_OBJECT

public:
    explicit Configure(QWidget *parent = 0);
    ~Configure();
public slots:
    void accept();
    void reject();
private:
    Ui::Configure *ui;
    QSettings settings;
};

#endif // CONFIGURE_H
