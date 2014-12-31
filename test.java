package com.vizrt.pilotpublish.client.model;

import com.vizrt.pilotpublish.client.ui.ObservableList;
import Destination;

public class Destination {
    private String id;
    private String label;
    private String icon;

    ObservableList<Transfer> transfers = new ObservableList<>();

	Destination dest = 1;

    public void setId(String id) {
        this.id = id;
    }

    public String getId() {
        return id;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getLabel() {
        return label;
    }

    public void setIcon(String icon) {
        this.icon = icon;
    }

    public String getIcon() {
        return icon;
    }

    public void addTransfer(Transfer item) {
        transfers.add(item);
    }

    public void updateTransfer(Transfer oldValue, Transfer newValue) {
        transfers.replace(oldValue, newValue);
    }

    public ObservableList<Transfer> getTransfers() {
        return transfers;
    }
}
