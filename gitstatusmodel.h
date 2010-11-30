#ifndef GITSTATUSMODEL_H
#define GITSTATUSMODEL_H

#include <QAbstractListModel>
#include <QStringList>
#include <QProcess>

class GitStatusModel : public QAbstractListModel
{
    Q_OBJECT
public:
    explicit GitStatusModel(QObject *parent = 0);
    ~GitStatusModel();
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;
signals:

public slots:
    void update(QString repo);
private slots:
    void readOutput(int exitCode, QProcess::ExitStatus exitStatus);

private:
        //void updateFileList();
        QProcess *process;
        QStringList *fileList;
};

#endif // GITSTATUSMODEL_H
