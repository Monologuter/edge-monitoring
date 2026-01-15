package com.safetyfire.monitor.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * 温湿度/液位采集值换算配置。
 */
@Component
@ConfigurationProperties(prefix = "app.device-reading.scale")
public class DeviceReadingScaleProperties {
    /**
     * 温度换算系数（原始值 / divisor）。
     */
    private double temperatureDivisor;

    /**
     * 湿度换算系数（原始值 / divisor）。
     */
    private double humidityDivisor;

    /**
     * 液位换算系数（原始值 / divisor）。
     */
    private double levelDivisor;

    public double getTemperatureDivisor() {
        return temperatureDivisor;
    }

    public void setTemperatureDivisor(double temperatureDivisor) {
        this.temperatureDivisor = temperatureDivisor;
    }

    public double getHumidityDivisor() {
        return humidityDivisor;
    }

    public void setHumidityDivisor(double humidityDivisor) {
        this.humidityDivisor = humidityDivisor;
    }

    public double getLevelDivisor() {
        return levelDivisor;
    }

    public void setLevelDivisor(double levelDivisor) {
        this.levelDivisor = levelDivisor;
    }
}
