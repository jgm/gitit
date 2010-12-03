#include "configure.h"
#include "ui_configure.h"
#include "gitcommand.h"
Configure::Configure(QWidget *parent, GitCommand *gitCommand) :
    QDialog(parent),
    ui(new Ui::Configure),
    gitCommand(gitCommand)
{
    ui->setupUi(this);
    ui->gitPathLineEdit->setText(settings.value("gitPath").toString());
    QStringList name = gitCommand->run(QStringList() << "config" << "user.name");
    if(name.size() > 0)
        ui->nameLineEdit->setText(name[0]);
    QStringList email = gitCommand->run(QStringList() << "config" << "user.email");
    if(email.size() > 0)
        ui->emailLineEdit->setText(email[0]);

}
Configure::~Configure()
{
    delete ui;
}
void Configure::accept()
{
    settings.setValue("gitPath",ui->gitPathLineEdit->text());
    gitCommand->run(QStringList() << "config" << "user.name" << ui->nameLineEdit->text());
    gitCommand->run(QStringList() << "config" << "user.email" << ui->emailLineEdit->text());
    this->hide();
}
void Configure::reject()
{
    ui->gitPathLineEdit->setText(settings.value("gitPath").toString());
    QStringList name = gitCommand->run(QStringList() << "config" << "user.name");
    if(name.size() > 0)
        ui->nameLineEdit->setText(name[0]);
    QStringList email = gitCommand->run(QStringList() << "config" << "user.email");
    if(email.size() > 0)
        ui->emailLineEdit->setText(email[0]);
    this->hide();
}
