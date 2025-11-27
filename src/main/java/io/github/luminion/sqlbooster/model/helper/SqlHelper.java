package io.github.luminion.sqlbooster.model.helper;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.core.BoosterParam;
import io.github.luminion.sqlbooster.model.api.ConditionNode;
import io.github.luminion.sqlbooster.util.BoostUtils;

/**
 * SQL 构建助手实现类.
 * <p>
 * 提供 SQL 查询构建的具体实现, 支持链式调用和 Lambda 表达式.
 *
 * @param <T> 实体类型
 * @author luminion
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SqlHelper<T> extends LambdaHelper<T, SqlHelper<T>> {

    /**
     * 创建一个新的 {@link SqlHelper} 实例.
     *
     * @param <T> 实体类型
     * @return {@link SqlHelper} 实例
     * @since 1.0.0
     */
    public static <T> SqlHelper<T> of() {
        return new SqlHelper<>();
    }

    /**
     * 创建一个与指定实体类绑定的 {@link SqlHelper} 实例.
     *
     * @param entityClass 实体类
     * @param <T>         实体类型
     * @return {@link SqlHelper} 实例
     * @since 1.0.0
     */
    public static <T> SqlHelper<T> of(Class<T> entityClass) {
        SqlHelper<T> sqlHelper = new SqlHelper<>();
        sqlHelper.entityClass = entityClass;
        return sqlHelper;
    }

    /**
     * 创建一个指定 {@link BoosterParam}的实例
     *
     * @param boosterParam 源 {@link BoosterParam} 实例
     * @param <T>     实体类型
     * @return {@link SqlHelper} 实例
     * @since 1.0.0
     */
    public static <T> SqlHelper<T> of(BoosterParam<T> boosterParam) {
        if (boosterParam == null) {
            return new SqlHelper<>();
        }
        if (boosterParam instanceof SqlHelper) {
            return (SqlHelper<T>) boosterParam;
        }
        if (boosterParam instanceof ConditionNode){
            ConditionNode conditionNode = (ConditionNode) boosterParam;
            SqlHelper<T> sqlHelper = new SqlHelper<>();
            sqlHelper.append(conditionNode);
        }
        SqlHelper<T> sqlHelper = new SqlHelper<>();
        return sqlHelper.append(boosterParam);
    }

    /**
     * 创建一个与 {@link Booster} 实例的实体类型绑定的 {@link SqlHelper} 实例.
     *
     * @param booster Booster 实例
     * @param <T>     实体类型
     * @param <R>     VO 类型
     * @return {@link SqlHelper} 实例
     * @since 1.0.0
     */
    public static <T, R> SqlHelper<T> of(Booster<T, R> booster) {
        return SqlHelper.of(BoostUtils.getEntityClass(booster));
    }

    /**
     * 转化为指定实体类对应的 {@link SqlHelper} 实例.
     *
     * @param entityClass 实体类
     * @return 当前 {@link SqlHelper} 实例
     * @since 1.0.0
     */
    @SuppressWarnings("unchecked")
    public <R> SqlHelper<R> convert(Class<R> entityClass) {
        SqlHelper<R> sqlHelper = (SqlHelper<R>) this;
        sqlHelper.entityClass = entityClass;
        return sqlHelper;
    }


    @Override
    public SqlHelper<T> newInstance() {
        return new SqlHelper<>();
    }
}