#ifndef GITSTATUSMODEL_H
#define GITSTATUSMODEL_H

#include <QAbstractListModel>
#include <QStringList>

class GitChangedStatusModel : public QAbstractListModel
{
    Q_OBJECT
public:
    explicit GitChangedStatusModel(QObject *parent = 0);
    ~GitChangedStatusModel();
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;
signals:

public slots:
    void update(QStringList files);

private:
        //void updateFileList();
        QStringList *fileList;
};

#endif // GITSTATUSMODEL_H
