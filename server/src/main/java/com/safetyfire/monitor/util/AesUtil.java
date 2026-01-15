package com.safetyfire.monitor.util;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

/**
 * AES加密工具类（用于省厅对接）。
 */
public class AesUtil {

    /**
     * AES加密（CBC模式，PKCS5Padding填充）。
     *
     * @param plainText 明文
     * @param key       Base64编码的密钥
     * @param iv        Base64编码的IV向量
     * @return Base64编码的密文
     */
    public static String encrypt(String plainText, String key, String iv) {
        try {
            // 解码Base64的key和iv
            byte[] keyBytes = Base64.getDecoder().decode(key);
            byte[] ivBytes = Base64.getDecoder().decode(iv);

            // 创建密钥和IV
            SecretKeySpec secretKeySpec = new SecretKeySpec(keyBytes, "AES");
            IvParameterSpec ivParameterSpec = new IvParameterSpec(ivBytes);

            // 创建加密器
            Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
            cipher.init(Cipher.ENCRYPT_MODE, secretKeySpec, ivParameterSpec);

            // 加密
            byte[] encrypted = cipher.doFinal(plainText.getBytes(StandardCharsets.UTF_8));

            // 返回Base64编码
            return Base64.getEncoder().encodeToString(encrypted);
        } catch (Exception e) {
            throw new RuntimeException("AES加密失败", e);
        }
    }

    /**
     * AES解密（CBC模式，PKCS5Padding填充）。
     *
     * @param cipherText Base64编码的密文
     * @param key        Base64编码的密钥
     * @param iv         Base64编码的IV向量
     * @return 明文
     */
    public static String decrypt(String cipherText, String key, String iv) {
        try {
            // 解码Base64的key、iv和密文
            byte[] keyBytes = Base64.getDecoder().decode(key);
            byte[] ivBytes = Base64.getDecoder().decode(iv);
            byte[] encryptedBytes = Base64.getDecoder().decode(cipherText);

            // 创建密钥和IV
            SecretKeySpec secretKeySpec = new SecretKeySpec(keyBytes, "AES");
            IvParameterSpec ivParameterSpec = new IvParameterSpec(ivBytes);

            // 创建解密器
            Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
            cipher.init(Cipher.DECRYPT_MODE, secretKeySpec, ivParameterSpec);

            // 解密
            byte[] decrypted = cipher.doFinal(encryptedBytes);

            return new String(decrypted, StandardCharsets.UTF_8);
        } catch (Exception e) {
            throw new RuntimeException("AES解密失败", e);
        }
    }
}
