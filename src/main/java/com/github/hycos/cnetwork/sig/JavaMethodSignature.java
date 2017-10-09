/*
 * cnetwork - a constraint network implementation for Java
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetwork is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */


package com.github.hycos.cnetwork.sig;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public final class JavaMethodSignature {

    public static final String JavaLangThreadStart = "java.lang.Thread.start()V";
    public static final String JavaLangThreadJoin = "java.lang.Thread.join()V";

    private final JavaType declaringType;
    private final String methodName;
    private final List<JavaType> argumentTypes;
    private final JavaType returnType;
    private final int hash;
    private final String bcString;
    private static final Pattern pMethodBC = Pattern
            .compile("(.+)\\((.*)\\)(.+)");
    private static final Pattern pMethodHR = Pattern
            .compile("(.+)\\s+(.+)\\((.*)\\)");

    private JavaMethodSignature(String methodName,
                                List<JavaType> argumentTypes, JavaType returnType) {
        this.methodName = methodName.substring(methodName.lastIndexOf('.') + 1);
        this.argumentTypes = new ArrayList<JavaType>(argumentTypes);
        this.returnType = returnType;
        this.declaringType = JavaType.parseSingleTypeFromString(methodName.substring(0, methodName.lastIndexOf('.')), JavaType.Format.HR);
        assert this.declaringType != null;
        this.hash = computeHashCode();
        this.bcString = makeBCString();
    }

    public String getMethodName() {
        return methodName;
    }

    public String getFullyQualifiedMethodName() {
        return declaringType.toHRString() + "." + getMethodName();
    }

    public JavaType getDeclaringType() {
        return declaringType;
    }

    public List<JavaType> getArgumentTypes() {
        return argumentTypes;
    }

    public JavaType getReturnType() {
        return returnType;
    }

    public String toHRString() {
        StringBuilder sbHR = new StringBuilder();
        sbHR.append(returnType.toHRString());
        sbHR.append(" ");
        sbHR.append(declaringType.toHRString());
        sbHR.append(".");
        sbHR.append(methodName);
        sbHR.append("(");
        for (int i = 0; i < argumentTypes.size(); i++) {
            JavaType nextType = argumentTypes.get(i);
            sbHR.append(nextType.toHRString());
            if (i < argumentTypes.size() - 1) {
                sbHR.append(", ");
            }
        }
        sbHR.append(")");
        return sbHR.toString();
    }

    public String toBCString() {
        return bcString;
    }

    private String makeBCString() {
        StringBuilder sbBC = new StringBuilder();
        sbBC.append(declaringType.toHRString());
        sbBC.append(".");
        sbBC.append(methodName);
        sbBC.append("(");
        for (int i = 0; i < argumentTypes.size(); i++) {
            sbBC.append(argumentTypes.get(i).toBCString());
        }
        sbBC.append(")");
        sbBC.append(returnType.toBCString());
        return sbBC.toString();
    }

    public String getSelector() {
        StringBuilder sbBC = new StringBuilder();
        sbBC.append(methodName);
        sbBC.append("(");
        for (int i = 0; i < argumentTypes.size(); i++) {
            sbBC.append(argumentTypes.get(i).toBCString());
        }
        sbBC.append(")");
        sbBC.append(returnType.toBCString());
        return sbBC.toString();
    }

    private static boolean isHumanReadable(String methodSig) {
        Matcher m = pMethodHR.matcher(methodSig);
        if (!m.matches()) {
            return false;
        } else {
            return true;
            /** m.group(1) -> return type
             m.group(2) -> declaring class + method name
             m.group(3) -> argument types
             */

        }
    }

    private static boolean isBytecode(String methodSig) {
        Matcher m = pMethodBC.matcher(methodSig);
        if (!m.matches()) {
            return false;
        } else {
            return true;
            /**
             * m.group(1) -> declaring class + method name
             * m.group(2) -> argument types
             * m.group(3) -> return type
             */
        }
    }

    public static JavaMethodSignature mainMethodOfClass(String className) {
        return fromString(className+(".main([Ljava/lang/String;)V"));
    }

    public static JavaMethodSignature fromString(String methodSig) {
        if (isHumanReadable(methodSig)) {
            Matcher m = pMethodHR.matcher(methodSig);
            if (m.matches()) {
                /**
                 * m.group(1) -> return type
                 * m.group(2) -> declaring class + method name
                 * m.group(3) -> argument types
                 */
                return new JavaMethodSignature(m.group(2), JavaType.parseListOfTypesFromString(m.group(3), JavaType.Format.HR), JavaType.parseSingleTypeFromString(m.group(1), JavaType.Format.HR));
            } else {
                throw new IllegalStateException();
            }
        } else if (isBytecode(methodSig)) {
            Matcher m = pMethodBC.matcher(methodSig);
            if (m.matches()) {
                /**
                 * m.group(1) -> declaring class + method name
                 * m.group(2) -> argument types
                 * m.group(3) -> return type
                 */
                return new JavaMethodSignature(m.group(1),
                        JavaType.parseListOfTypesFromString(m.group(2), JavaType.Format.BC), JavaType.parseSingleTypeFromString(m.group(3), JavaType.Format.BC));
            } else {
                throw new IllegalStateException();
            }
        } else {
            throw new IllegalArgumentException(methodSig);
        }
    }

    public static Collection<? extends JavaType> extractDeclaringClasses(Collection<? extends JavaMethodSignature> sigs) {
        List<JavaType> ret = new LinkedList<JavaType>();
        for (JavaMethodSignature sig : sigs) {
            ret.add(sig.getDeclaringType());
        }
        return ret;
    }


    @Override
    public String toString() {
        return toHRString();
    }

    public String toStringHRShort() {
        StringBuilder sbHR = new StringBuilder();
        sbHR.append(returnType.toHRString());
        sbHR.append(" ");
        sbHR.append(methodName);
        sbHR.append("(");
        for (int i = 0; i < argumentTypes.size(); i++) {
            sbHR.append(argumentTypes.get(i).toHRString());
            if (i < argumentTypes.size() - 1)
                sbHR.append(", ");
        }
        sbHR.append(")");
        return sbHR.toString();
    }

    private int computeHashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((argumentTypes == null) ? 0 : argumentTypes.hashCode());
        result = prime * result
                + ((declaringType == null) ? 0 : declaringType.hashCode());
        result = prime * result
                + ((methodName == null) ? 0 : methodName.hashCode());
        result = prime * result
                + ((returnType == null) ? 0 : returnType.hashCode());
        return result;
    }

    @Override
    public int hashCode() {
        return hash;
    }


    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof JavaMethodSignature)) {
            return false;
        }
        JavaMethodSignature other = (JavaMethodSignature) obj;
        return this.bcString.equals(other.bcString);
    }

    public static Comparator<JavaMethodSignature> compareByBCStrings() {
        return new Comparator<JavaMethodSignature>() {

            @Override
            public int compare(JavaMethodSignature o1, JavaMethodSignature o2) {
                return o1.toBCString().compareTo(o2.toBCString());
            }

        };
    }
}