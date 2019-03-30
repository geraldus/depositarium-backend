export const setUserData = (formData, user) => {
    formData.append("lastName", user.lastName)
    formData.append("firstName", user.firstName)
    formData.append("patronymic", user.patronymic)
    formData.append("ident", user.ident)
    formData.append("email", user.email)
    formData.append("password", user.password)
    formData.append("rights", JSON.stringify(user.rights))
}

