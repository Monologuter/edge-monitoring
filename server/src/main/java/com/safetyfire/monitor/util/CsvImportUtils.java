package com.safetyfire.monitor.util;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * 简单 CSV 解析工具（UTF-8，逗号分隔，不支持字段内逗号）。
 */
public final class CsvImportUtils {
    private CsvImportUtils() {
    }

    public static List<Map<String, String>> parse(InputStream inputStream) {
        List<Map<String, String>> rows = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))) {
            String headerLine = reader.readLine();
            if (headerLine == null) return rows;
            String[] headers = splitLine(headerLine);
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.trim().isEmpty()) continue;
                String[] values = splitLine(line);
                Map<String, String> row = new HashMap<>();
                for (int i = 0; i < headers.length; i++) {
                    String key = headers[i].trim();
                    if (key.isEmpty()) continue;
                    String val = i < values.length ? values[i].trim() : "";
                    row.put(key, val);
                }
                rows.add(row);
            }
        } catch (Exception e) {
            throw new IllegalArgumentException("CSV 解析失败：" + e.getMessage());
        }
        return rows;
    }

    private static String[] splitLine(String line) {
        return line.split(",", -1);
    }

    public static String firstNonBlank(Map<String, String> row, String... keys) {
        for (String k : keys) {
            String v = row.get(k);
            if (v != null && !v.isBlank()) return v.trim();
        }
        return "";
    }
}
