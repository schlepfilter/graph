path = require("path");

js = "js/";

module.exports = {
    entry: "./src/js/index.js",
    module: {
        rules: [
            {
                test: /\.css$/,
                use: ["style-loader", "css-loader"],
            },
            {
                test: /\.(ttf|woff|woff2)$/,
                use: ["file-loader"],
            }
        ]
    },
    output: {
        path: path.resolve(__dirname, "resources/public", js),
        filename: "index_bundle.js",
        publicPath: js
    }
};
