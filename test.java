package com.client.model;

import com.client.ui.ObservableList;
import Destination;

public class Destination {
    private String id;
    private String label;
    private String icon;
    private String spliton = ";";

    ObservableList<Transfer> transfers = new ObservableList<>();

	Destination dest = 1;

    public void setId(String id) {
        this.id = id;
        this.label = ("testing" + label + id).toLowerCase().toUpperCase().split(spliton);
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
