package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.core.Booster;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.builder.xml.XMLMapperBuilder;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSessionFactory;

import java.io.IOException;
import java.io.InputStream;

/**
 * Mapper 工具类.
 * <p>
 * 提供 Mapper 相关的工具方法, 包括 SQL 片段的初始化和动态 Mapper 内容的生成.
 *
 * @author luminion
 * @since 1.0.0
 */
@Slf4j
public abstract class MapperUtils {

    /**
     * 初始化核心 SQL 片段.
     * <p>
     * 从资源文件中加载 SQL 片段,并注册到 MyBatis 的 {@link Configuration} 中.
     *
     * @param sqlSessionFactory SQL 会话工厂
     * @return 如果初始化成功返回 true, 否则返回 false
     * @since 1.0.0
     */
    public static boolean initSqlFragment(SqlSessionFactory sqlSessionFactory) {
        Configuration configuration = sqlSessionFactory.getConfiguration();
        String resource = "booster/booster.xml";
        try (InputStream inputStream = Resources.getResourceAsStream(resource)) {
            XMLMapperBuilder mapperBuilder = new XMLMapperBuilder(inputStream, configuration, resource, configuration.getSqlFragments());
            mapperBuilder.parse();
            return true;
        } catch (IOException e) {
            log.error("error creating sqlFragments", e);
            return false;
        }
    }

    /**
     * 根据 {@link Booster} 实现类生成 Mapper XML 的 select 语句内容.
     *
     * @param boostClass 实现了 Booster 接口的类
     * @param <T>        实体类型
     * @param <V>        VO 类型
     * @return 生成的 Mapper select 语句字符串
     * @throws IllegalArgumentException 当无法解析实体信息时抛出
     * @since 1.0.0
     */
    public static <T, V> String getMapperContent(Class<? extends Booster<T, V>> boostClass) {
        Class<?>[] classes = ReflectUtils.resolveTypeArguments(boostClass, Booster.class);
        Class<?> entityClass = classes[0];
        Class<?> voClass = classes[1];
        return getMapperContent(entityClass, voClass);
    }

    /**
     * 根据实体类和 VO 类生成 Mapper XML 的 select 语句内容.
     *
     * @param entityClass 实体类
     * @param voClass     VO 类
     * @param <T>         实体类型
     * @return 生成的 Mapper select 语句字符串
     * @since 1.0.0
     */
    public static <T> String getMapperContent(Class<T> entityClass, Class<?> voClass) {
        return "    <select id=\"selectByBooster\" resultType=\"" + voClass.getName() + "\">\n" +
                getSqlContent(entityClass) +
                "    </select>";
    }

    /**
     * 根据实体类生成 SQL 查询语句的核心内容.
     *
     * @param entityClass 实体类
     * @param <T>         实体类型
     * @return 生成的 SQL 语句字符串
     * @since 1.0.0
     */
    public static <T> String getSqlContent(Class<T> entityClass) {
        String tableName;
        try {
            tableName = TableInfoUtils.getTableName(entityClass);
        } catch (IllegalStateException e) {
            tableName = TableInfoUtils.camelCaseToUnderscore(entityClass.getName());
            if (tableName.startsWith("_")) {
                return tableName.substring(1);
            }
        }
        return getSqlContent(tableName);
    }

    /**
     * 根据表名生成 SQL 查询语句的核心内容.
     * <p>
     *
     * @param tableName 数据库表名
     * @return 生成的 SQL 语句字符串
     * @since 1.0.0
     */
    public static String getSqlContent(String tableName) {
        return "        SELECT\n" +
                "        a.*\n" +
                "        FROM\n" +
                "        " + tableName + " a\n" +
                "        <where>\n" +
                "            <include refid=\"sqlbooster.conditions\"/>\n" +
                "        </where>\n" +
                "        <trim prefix=\"ORDER BY\" prefixOverrides=\",\">\n" +
                "            <include refid=\"sqlbooster.sorts\"/>\n" +
                "        </trim>";
    }


}