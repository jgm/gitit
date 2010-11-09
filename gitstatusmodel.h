#ifndef GITSTATUSMODEL_H
#define GITSTATUSMODEL_H

#include <QAbstractListModel>
#include <QStringList>

class GitStatusModel : public QAbstractListModel
{
    Q_OBJECT
public:
    explicit GitStatusModel(QObject *parent = 0);
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;
signals:

public slots:

private:
        QStringList fileList;
        int fileCount;
};

#endif // GITSTATUSMODEL_H
