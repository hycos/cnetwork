package com.github.hycos.cnetwork.core;

/**
 * Created by julian on 15/06/2017.
 */
public enum Configuration {
    INSTACE;

    boolean eufEnabled = true;
    boolean preprocEnabled = true;
    boolean postprocEnabled = true;

    public boolean isPostprocEnabled() {
        return postprocEnabled;
    }

    public void setPostprocEnabled(boolean postprocEnabled) {
        this.postprocEnabled = postprocEnabled;
    }

    public void setEufEnabled(boolean eufEnabled) {
        this.eufEnabled = eufEnabled;
    }

    public boolean isEufEnabled() {
        return eufEnabled;
    }

    public void setPreprocEnabled(boolean preprocEnabled) {
        this.preprocEnabled = preprocEnabled;
    }

    public boolean isPreprocEnabled() {
        return preprocEnabled;
    }
}
