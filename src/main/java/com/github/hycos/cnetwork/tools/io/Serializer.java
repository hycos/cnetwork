package com.github.hycos.cnetwork.tools.io;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;

public enum Serializer {

    INSTANCE;

    final static Logger LOGGER = LoggerFactory.getLogger(Serializer.class);




    public byte[] toByteArray(Object cn) {

        ObjectOutputStream o_out = null;
        ByteArrayOutputStream b_out = new ByteArrayOutputStream();


        try {
            o_out = new ObjectOutputStream(b_out);
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            return null;
        }

        try {
            o_out.writeObject(cn);
            o_out.flush();

        } catch (IOException e) {
            System.out.println(e.toString());
        } finally {
            try {
                b_out.close();
            } catch (IOException e) {
                LOGGER.error(e.getMessage());
                return null;
            }
            try {
                o_out.close();
            } catch (IOException e) {
                LOGGER.error(e.getMessage());
                return null;
            }
        }

        return b_out.toByteArray();
    }


    public Object byteArrayToObject(byte[] arr) {
        ObjectInputStream ois = null;

        try {
            ois = new ObjectInputStream(new ByteArrayInputStream(arr));
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
            return null;
        }

        Object o = null;

        try {
            o = ois.readObject();
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
            return null;
        } catch (ClassNotFoundException e) {
            LOGGER.error(e.getMessage());
            return null;
        } finally {
            try {
                ois.close();
            } catch (IOException e) {
                LOGGER.error(e.getMessage());
                return null;
            }
        }

        return o;
    }


}
