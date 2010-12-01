#ifndef GITCOMMAND_H
#define GITCOMMAND_H


#include <QObject>
#include <QStringList>
#include <QString>
#include <QProcess>
class GitCommand : public QObject
{
    Q_OBJECT
public:
    explicit GitCommand(QObject *parent = 0);
    ~GitCommand();
    void status();

private:
    QStringList* fileList;
    QString repo;
    QProcess* gitStatusProcess;

signals:
    void status(QStringList files);

public slots:
    void setRepo(QString repo);

private slots:
    void statusOutput(int exitCode, QProcess::ExitStatus exitStatus);
};

#endif // GITCOMMAND_H
