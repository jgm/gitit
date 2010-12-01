#include "configure.h"
#include "ui_configure.h"

Configure::Configure(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::Configure)
{
    ui->setupUi(this);
    ui->gitPathLineEdit->setText(settings.value("gitPath").toString());
}

Configure::~Configure()
{
    delete ui;
}
void Configure::accept()
{
    settings.setValue("gitPath",ui->gitPathLineEdit->text());
    this->hide();
}
void Configure::reject()
{
    ui->gitPathLineEdit->setText(settings.value("gitPath").toString());
    this->hide();
}
