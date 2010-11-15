#ifndef GITSTATUSMODEL_H
#define GITSTATUSMODEL_H

#include <QAbstractListModel>
#include <QStringList>
#include "git/repository.h"
class GitStatusModel : public QAbstractListModel
{
    Q_OBJECT
public:
    explicit GitStatusModel(QObject *parent = 0);
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;
signals:

public slots:
    void update(git_repository* repo);

private:
        void updateFileList();
        QStringList fileList;
        int fileCount;
        git_index* gitIndex;
};

#endif // GITSTATUSMODEL_H
